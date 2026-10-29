/*
 * HỆ THỐNG QUẢN LÝ SIÊU THỊ MINI & HÓA ĐƠN BÁN LẺ (POS)
 * -------------------------------------------------------
 * Cấu trúc dữ liệu : MatHang, ChiTietHoaDon, HoaDon
 * Thuật toán       : Binary search theo mã hàng O(log N), tính tiền/VAT/tiền thối
 * Nghiệp vụ        : quét mã, in phiếu thu, trừ tồn kho tự động, thống kê theo ca
 * Kỹ thuật C       : mảng động (malloc/realloc), ghi file an toàn (file tạm + rename)
 *
 * Biên dịch: gcc -Wall -Wextra -O2 pos_sieuthi.c -o pos
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#ifdef _WIN32
#define strncasecmp _strnicmp
#else
#include <strings.h>
#endif

#define MAX_MA        20
#define MAX_TEN       64
#define MAX_NV        32
#define LINE_W        48
#define FILE_HANG     "hanghoa.txt"
#define FILE_LOG      "doanhthu.log"

typedef long long Tien;

/* ======================= CẤU TRÚC DỮ LIỆU ======================= */
typedef struct {
    char ma[MAX_MA];      /* mã vạch / mã sản phẩm (khoá tìm kiếm) */
    char ten[MAX_TEN];
    Tien donGia;          /* giá chưa VAT */
    int  tonKho;
    int  vat;             /* % thuế VAT: 0, 5, 8, 10 */
} MatHang;

typedef struct {
    char ma[MAX_MA];
    char ten[MAX_TEN];
    int  soLuong;
    Tien donGia;
    int  vat;
    Tien thanhTien;       /* soLuong * donGia (chưa VAT) */
    Tien tienVAT;
} ChiTietHoaDon;

typedef struct {
    int    soHD;
    time_t thoiGian;
    int    ca;                    /* 1..3 */
    char   nhanVien[MAX_NV];
    ChiTietHoaDon *ct;            /* mảng động */
    int    soDong, sucChua;
    Tien   tongTien, tongVAT, tongThanhToan, khachDua, tienThoi;
} HoaDon;

typedef struct { MatHang *ds; int n, cap; } KhoHang;   /* luôn sắp xếp theo mã */
typedef struct { int ca; char nhanVien[MAX_NV]; } CaLamViec;

static const char *TEN_CA[] = {"", "Ca sáng (06-14h)", "Ca chiều (14-22h)", "Ca tối (22-06h)"};

/* ======================= TIỆN ÍCH ======================= */
static int u8len(const char *s) { int n = 0; for (; *s; s++) if (((unsigned char)*s & 0xC0) != 0x80) n++; return n; }
static void padR(FILE *f, const char *s, int w) { fputs(s, f); for (int i = u8len(s); i < w; i++) fputc(' ', f); }
static void padL(FILE *f, const char *s, int w) { for (int i = u8len(s); i < w; i++) fputc(' ', f); fputs(s, f); }
static void center(FILE *f, const char *s) { int sp = (LINE_W - u8len(s)) / 2; for (int i = 0; i < sp; i++) fputc(' ', f); fputs(s, f); fputc('\n', f); }
static void gach(int n) { for (int i = 0; i < n; i++) putchar('-'); putchar('\n'); }
static void hr(FILE *f, char c) { for (int i = 0; i < LINE_W; i++) fputc(c, f); fputc('\n', f); }

/* cắt chuỗi UTF-8 tối đa maxc ký tự */
static void u8cut(char *dst, const char *src, int maxc) {
    int c = 0; while (*src) {
        if (((unsigned char)*src & 0xC0) != 0x80) { if (c == maxc) break; c++; }
        *dst++ = *src++;
    } *dst = 0;
}

/* 1234567 -> "1.234.567" (vòng đệm 8 chuỗi để gọi nhiều lần trong 1 printf) */
static const char *money(Tien v) {
    static char buf[8][32]; static int k = 0;
    char *out = buf[k = (k + 1) & 7], tmp[32]; int neg = v < 0; if (neg) v = -v;
    snprintf(tmp, sizeof tmp, "%lld", v);
    int len = (int)strlen(tmp), o = 0; if (neg) out[o++] = '-';
    for (int i = 0; i < len; i++) { if (i && (len - i) % 3 == 0) out[o++] = '.'; out[o++] = tmp[i]; }
    out[o] = 0; return out;
}

/* Đọc 1 dòng an toàn, bỏ '\n'. Trả 0 nếu EOF. */
static int docDong(const char *nhac, char *buf, size_t size) {
    if (nhac) { printf("%s", nhac); fflush(stdout); }
    if (!fgets(buf, (int)size, stdin)) return 0;
    size_t l = strlen(buf);
    if (l && buf[l - 1] == '\n') buf[--l] = 0;
    else { int ch; while ((ch = getchar()) != '\n' && ch != EOF) {} }   /* bỏ phần dư */
    while (l && isspace((unsigned char)buf[l - 1])) buf[--l] = 0;
    return 1;
}

/* Đọc số nguyên trong [lo, hi]; lặp đến khi hợp lệ. Trả 0 nếu EOF. */
static int docSo(const char *nhac, long long lo, long long hi, long long *out) {
    char buf[64];
    for (;;) {
        if (!docDong(nhac, buf, sizeof buf)) return 0;
        char *e; long long v = strtoll(buf, &e, 10);
        if (e != buf && *e == 0 && v >= lo && v <= hi) { *out = v; return 1; }
        printf("  ! Nhập số nguyên trong khoảng %lld..%lld\n", lo, hi);
    }
}

static void chuanHoaMa(char *s) {   /* trim + IN HOA, bỏ '|' */
    char *p = s; while (isspace((unsigned char)*p)) p++;
    memmove(s, p, strlen(p) + 1);
    for (char *q = s; *q; q++) { *q = (char)toupper((unsigned char)*q); if (*q == '|') *q = '-'; }
}
static void lamSachTen(char *s) { for (; *s; s++) if (*s == '|' || *s == '\n' || *s == '\r') *s = '-'; }

/* ======================= KHO HÀNG (mảng động đã sắp xếp) ======================= */
static int cmpHang(const void *a, const void *b) { return strcmp(((const MatHang *)a)->ma, ((const MatHang *)b)->ma); }

/* lower_bound: vị trí đầu tiên có ma >= key — O(log N) */
static int kho_lowerBound(const KhoHang *k, const char *ma) {
    int lo = 0, hi = k->n;
    while (lo < hi) { int mid = lo + (hi - lo) / 2; if (strcmp(k->ds[mid].ma, ma) < 0) lo = mid + 1; else hi = mid; }
    return lo;
}
/* Tìm nhị phân theo mã: trả chỉ số hoặc -1 — O(log N) */
static int kho_tim(const KhoHang *k, const char *ma) {
    int p = kho_lowerBound(k, ma);
    return (p < k->n && strcmp(k->ds[p].ma, ma) == 0) ? p : -1;
}
static int kho_them(KhoHang *k, const MatHang *mh) {   /* chèn giữ thứ tự; 0 = ok, -1 = lỗi/trùng */
    int p = kho_lowerBound(k, mh->ma);
    if (p < k->n && strcmp(k->ds[p].ma, mh->ma) == 0) return -1;
    if (k->n == k->cap) {
        int nc = k->cap ? k->cap * 2 : 16;
        MatHang *t = realloc(k->ds, (size_t)nc * sizeof *t);
        if (!t) return -1;
        k->ds = t; k->cap = nc;
    }
    memmove(&k->ds[p + 1], &k->ds[p], (size_t)(k->n - p) * sizeof *k->ds);
    k->ds[p] = *mh; k->n++;
    return 0;
}

/* Ghi an toàn: ghi ra file tạm -> kiểm tra lỗi -> rename đè file thật */
static int kho_luu(const KhoHang *k) {
    char tmp[] = FILE_HANG ".tmp";
    FILE *f = fopen(tmp, "w");
    if (!f) { perror("  ! Không tạo được file tạm"); return -1; }
    for (int i = 0; i < k->n; i++)
        if (fprintf(f, "%s|%s|%lld|%d|%d\n", k->ds[i].ma, k->ds[i].ten, k->ds[i].donGia, k->ds[i].tonKho, k->ds[i].vat) < 0) {
            fclose(f); remove(tmp); return -1;
        }
    if (fflush(f) != 0 || ferror(f)) { fclose(f); remove(tmp); return -1; }
    if (fclose(f) != 0) { remove(tmp); return -1; }
    remove(FILE_HANG);                       /* Windows: rename không ghi đè được */
    if (rename(tmp, FILE_HANG) != 0) { perror("  ! rename"); return -1; }
    return 0;
}

static void kho_duLieuMau(KhoHang *k) {
    static const MatHang mau[] = {
        {"8934563138165", "Mì Hảo Hảo tôm chua cay",   4500, 200, 8},
        {"8935001700012", "Nước suối Aquafina 500ml",  6000, 150, 8},
        {"8934588012044", "Sữa tươi Vinamilk 1L",     32000,  60, 5},
        {"8936036020115", "Coca-Cola lon 330ml",      10000, 120, 8},
        {"8934673102308", "Bánh Oreo 133g",           18000,  80, 8},
        {"8935024140014", "Dầu ăn Simply 1L",        58000,  40,10},
        {"8938505970012", "Gạo ST25 5kg",           125000,  25, 0},
        {"8936079130015", "Khăn giấy Pulppy",        15000,  90,10},
        {"8934804022019", "Cà phê G7 hộp 18 gói",    42000,  50, 8},
        {"8935049510022", "Xà phòng Lifebuoy 90g",   12000,  70,10},
    };
    for (size_t i = 0; i < sizeof mau / sizeof *mau; i++) kho_them(k, &mau[i]);
}

static void kho_doc(KhoHang *k) {
    FILE *f = fopen(FILE_HANG, "r");
    if (!f) { printf("Chưa có %s — tạo dữ liệu mẫu.\n", FILE_HANG); kho_duLieuMau(k); kho_luu(k); return; }
    char dong[256]; int soDong = 0, loi = 0;
    while (fgets(dong, sizeof dong, f)) {
        soDong++;
        MatHang m; memset(&m, 0, sizeof m);
        long long gia; int ton, vat;
        if (sscanf(dong, "%19[^|]|%63[^|]|%lld|%d|%d", m.ma, m.ten, &gia, &ton, &vat) != 5
            || gia < 0 || ton < 0 || vat < 0 || vat > 100) { loi++; continue; }
        m.donGia = gia; m.tonKho = ton; m.vat = vat;
        chuanHoaMa(m.ma);
        if (kho_them(k, &m) != 0) loi++;          /* trùng mã cũng tính là dòng lỗi */
    }
    fclose(f);
    if (loi) printf("Cảnh báo: bỏ qua %d/%d dòng lỗi hoặc trùng mã trong %s\n", loi, soDong, FILE_HANG);
}

/* ======================= HÓA ĐƠN ======================= */
static int hd_khoiTao(HoaDon *h, int soHD, const CaLamViec *ca) {
    memset(h, 0, sizeof *h);
    h->sucChua = 4;
    h->ct = malloc((size_t)h->sucChua * sizeof *h->ct);
    if (!h->ct) return -1;
    h->soHD = soHD; h->ca = ca->ca; h->thoiGian = time(NULL);
    snprintf(h->nhanVien, sizeof h->nhanVien, "%s", ca->nhanVien);
    return 0;
}
static void hd_giaiPhong(HoaDon *h) { free(h->ct); h->ct = NULL; h->soDong = h->sucChua = 0; }

static int hd_soLuongTrongGio(const HoaDon *h, const char *ma) {
    for (int i = 0; i < h->soDong; i++) if (strcmp(h->ct[i].ma, ma) == 0) return h->ct[i].soLuong;
    return 0;
}

static void hd_tinh(HoaDon *h) {
    h->tongTien = h->tongVAT = 0;
    for (int i = 0; i < h->soDong; i++) {
        ChiTietHoaDon *c = &h->ct[i];
        c->thanhTien = c->donGia * c->soLuong;
        c->tienVAT   = (c->thanhTien * c->vat + 50) / 100;      /* làm tròn VND */
        h->tongTien += c->thanhTien; h->tongVAT += c->tienVAT;
    }
    h->tongThanhToan = h->tongTien + h->tongVAT;
}

static int hd_themDong(HoaDon *h, const MatHang *mh, int sl) {
    for (int i = 0; i < h->soDong; i++)
        if (strcmp(h->ct[i].ma, mh->ma) == 0) { h->ct[i].soLuong += sl; hd_tinh(h); return 0; }
    if (h->soDong == h->sucChua) {                 /* mảng đầy -> nhân đôi */
        int nc = h->sucChua * 2;
        ChiTietHoaDon *t = realloc(h->ct, (size_t)nc * sizeof *t);
        if (!t) return -1;
        h->ct = t; h->sucChua = nc;
    }
    ChiTietHoaDon *c = &h->ct[h->soDong++];
    snprintf(c->ma, sizeof c->ma, "%s", mh->ma);
    snprintf(c->ten, sizeof c->ten, "%s", mh->ten);
    c->soLuong = sl; c->donGia = mh->donGia; c->vat = mh->vat;
    hd_tinh(h);
    return 0;
}

/* In phiếu thu chi tiết ra stdout hoặc file */
static void hd_in(const HoaDon *h, FILE *f) {
    char ngay[32]; struct tm *tm = localtime(&h->thoiGian);
    strftime(ngay, sizeof ngay, "%d/%m/%Y %H:%M:%S", tm);
    char s[160];

    hr(f, '=');
    center(f, "SIÊU THỊ MINI TIỆN LỢI");
    center(f, "123 Nguyễn Huệ, Q.1, TP. Hồ Chí Minh");
    center(f, "ĐT: 028 1234 5678 - MST: 0312345678");
    hr(f, '=');
    center(f, "PHIẾU THANH TOÁN");
    fprintf(f, "Số HĐ    : HD%06d\n", h->soHD);
    fprintf(f, "Thời gian: %s\n", ngay);
    fprintf(f, "Thu ngân : %s\n", h->nhanVien);
    fprintf(f, "Ca       : %s\n", TEN_CA[h->ca]);
    hr(f, '-');
    padR(f, "Mặt hàng", LINE_W - 14); padL(f, "Thành tiền", 14); fputc('\n', f);
    hr(f, '-');
    for (int i = 0; i < h->soDong; i++) {
        const ChiTietHoaDon *c = &h->ct[i];
        char ten[MAX_TEN]; u8cut(ten, c->ten, LINE_W - 4);
        fprintf(f, "%2d. %s\n", i + 1, ten);
        snprintf(s, sizeof s, "      %d x %s  (VAT %d%%)", c->soLuong, money(c->donGia), c->vat);
        padR(f, s, LINE_W - 14); padL(f, money(c->thanhTien), 14); fputc('\n', f);
    }
    hr(f, '-');
    padR(f, "Cộng tiền hàng", LINE_W - 14);  padL(f, money(h->tongTien), 14); fputc('\n', f);
    padR(f, "Thuế VAT", LINE_W - 14);        padL(f, money(h->tongVAT), 14);  fputc('\n', f);
    hr(f, '-');
    padR(f, "TỔNG THANH TOÁN (VND)", LINE_W - 14); padL(f, money(h->tongThanhToan), 14); fputc('\n', f);
    padR(f, "Khách đưa", LINE_W - 14);       padL(f, money(h->khachDua), 14); fputc('\n', f);
    padR(f, "Tiền thối", LINE_W - 14);       padL(f, money(h->tienThoi), 14); fputc('\n', f);
    hr(f, '=');
    center(f, "Cảm ơn quý khách - Hẹn gặp lại!");
    center(f, "Hàng đã mua vui lòng giữ phiếu để đổi trả trong 24h");
    hr(f, '=');
}

static int hd_luuFile(const HoaDon *h) {
    char ten[64]; snprintf(ten, sizeof ten, "hoadon_%06d.txt", h->soHD);
    FILE *f = fopen(ten, "w");
    if (!f) { perror("  ! Không tạo được file hóa đơn"); return -1; }
    hd_in(h, f);
    if (fflush(f) != 0 || ferror(f)) { fclose(f); return -1; }
    if (fclose(f) != 0) return -1;
    printf("  -> Đã lưu phiếu thu: %s\n", ten);
    return 0;
}

static int log_ghi(const HoaDon *h) {
    FILE *f = fopen(FILE_LOG, "a");
    if (!f) { perror("  ! Không mở được file doanh thu"); return -1; }
    char d[16], t[16]; struct tm *tm = localtime(&h->thoiGian);
    strftime(d, sizeof d, "%Y-%m-%d", tm); strftime(t, sizeof t, "%H:%M:%S", tm);
    char nv[MAX_NV]; snprintf(nv, sizeof nv, "%s", h->nhanVien); lamSachTen(nv);
    int rc = fprintf(f, "%d|%s|%s|%d|%s|%lld|%lld|%lld\n", h->soHD, d, t, h->ca, nv,
                     h->tongTien, h->tongVAT, h->tongThanhToan);
    if (fflush(f) != 0 || rc < 0) { fclose(f); return -1; }
    return fclose(f) == 0 ? 0 : -1;
}

static int log_soHDLonNhat(void) {
    FILE *f = fopen(FILE_LOG, "r"); if (!f) return 0;
    char dong[256]; int mx = 0, so;
    while (fgets(dong, sizeof dong, f)) if (sscanf(dong, "%d|", &so) == 1 && so > mx) mx = so;
    fclose(f); return mx;
}

/* ======================= NGHIỆP VỤ ======================= */
static void moCa(CaLamViec *ca) {
    long long c;
    printf("\n--- MỞ CA LÀM VIỆC ---\n1. %s\n2. %s\n3. %s\n", TEN_CA[1], TEN_CA[2], TEN_CA[3]);
    if (!docSo("Chọn ca [1-3]: ", 1, 3, &c)) exit(0);
    ca->ca = (int)c;
    do { if (!docDong("Tên thu ngân: ", ca->nhanVien, sizeof ca->nhanVien)) exit(0); } while (!ca->nhanVien[0]);
    lamSachTen(ca->nhanVien);
    printf("Đã mở %s — thu ngân: %s\n", TEN_CA[ca->ca], ca->nhanVien);
}

static void inDanhMuc(const KhoHang *k) {
    printf("\n%-15s", "Mã hàng"); padR(stdout, "Tên hàng", 28); padL(stdout, "Đơn giá", 10); padL(stdout, "Tồn", 6); padL(stdout, "VAT", 5); printf("\n");
    gach(64);
    for (int i = 0; i < k->n; i++) {
        char ten[MAX_TEN]; u8cut(ten, k->ds[i].ten, 27);
        printf("%-15s", k->ds[i].ma); padR(stdout, ten, 28);
        padL(stdout, money(k->ds[i].donGia), 10);
        char b[16]; snprintf(b, sizeof b, "%d", k->ds[i].tonKho); padL(stdout, b, 6);
        snprintf(b, sizeof b, "%d%%", k->ds[i].vat); padL(stdout, b, 5);
        printf("%s\n", k->ds[i].tonKho <= 10 ? "  <- sắp hết" : "");
    }
    printf("Tổng %d mặt hàng.\n", k->n);
}

static void inGioHang(const HoaDon *h) {
    printf("  Giỏ hàng: %d dòng | Tạm tính: %s | VAT: %s | Cộng: %s VND\n",
           h->soDong, money(h->tongTien), money(h->tongVAT), money(h->tongThanhToan));
}

static void banHang(KhoHang *k, const CaLamViec *ca, int *soHDKe) {
    HoaDon hd;
    if (hd_khoiTao(&hd, *soHDKe + 1, ca) != 0) { printf("Hết bộ nhớ!\n"); return; }
    printf("\n=== BÁN HÀNG — HD%06d ===\n", hd.soHD);
    printf("Quét/nhập: <mã> [số lượng] | ENTER = thanh toán | DEL <mã> = xóa dòng | HUY = hủy hóa đơn\n");

    char dong[128];
    for (;;) {
        if (!docDong("Mã > ", dong, sizeof dong)) { hd_giaiPhong(&hd); return; }
        if (!dong[0]) break;

        char ma[MAX_MA + 8] = ""; int sl = 1;
        if (strncmp(dong, "HUY", 3) == 0 || strncmp(dong, "huy", 3) == 0) {
            printf("  Đã hủy hóa đơn (tồn kho không đổi).\n"); hd_giaiPhong(&hd); return;
        }
        if (strncasecmp(dong, "DEL ", 4) == 0) {
            sscanf(dong + 4, "%19s", ma); chuanHoaMa(ma);
            int found = 0;
            for (int i = 0; i < hd.soDong; i++) if (strcmp(hd.ct[i].ma, ma) == 0) {
                memmove(&hd.ct[i], &hd.ct[i + 1], (size_t)(hd.soDong - i - 1) * sizeof *hd.ct);
                hd.soDong--; found = 1; break;
            }
            hd_tinh(&hd); printf(found ? "  Đã xóa.\n" : "  ! Không có mã này trong giỏ.\n"); inGioHang(&hd); continue;
        }
        char tail[64] = "";
        int nf = sscanf(dong, "%19s %63s", ma, tail);
        if (nf < 1) continue;
        if (nf == 2) {
            char *e; long v = strtol(tail, &e, 10);
            if (*e || v < 1 || v > 9999) { printf("  ! Số lượng không hợp lệ (1..9999).\n"); continue; }
            sl = (int)v;
        }
        chuanHoaMa(ma);

        int idx = kho_tim(k, ma);                    /* tìm nhị phân O(log N) */
        if (idx < 0) { printf("  ! Không tìm thấy mã '%s'.\n", ma); continue; }
        MatHang *mh = &k->ds[idx];
        int conLai = mh->tonKho - hd_soLuongTrongGio(&hd, ma);
        if (sl > conLai) { printf("  ! Không đủ tồn kho: '%s' chỉ còn %d.\n", mh->ten, conLai); continue; }
        if (hd_themDong(&hd, mh, sl) != 0) { printf("  ! Hết bộ nhớ.\n"); continue; }
        printf("  + %s x%d @ %s\n", mh->ten, sl, money(mh->donGia));
        inGioHang(&hd);
    }

    if (hd.soDong == 0) { printf("  Giỏ trống — hủy hóa đơn.\n"); hd_giaiPhong(&hd); return; }

    /* ---- Thanh toán ---- */
    hd_tinh(&hd);
    printf("\n  Cộng tiền hàng : %15s\n  Thuế VAT       : %15s\n  PHẢI THANH TOÁN: %15s VND\n",
           money(hd.tongTien), money(hd.tongVAT), money(hd.tongThanhToan));
    long long khach;
    for (;;) {
        if (!docSo("  Khách đưa (0 = hủy): ", 0, 100000000000LL, &khach)) { hd_giaiPhong(&hd); return; }
        if (khach == 0) { printf("  Đã hủy thanh toán.\n"); hd_giaiPhong(&hd); return; }
        if (khach >= hd.tongThanhToan) break;
        printf("  ! Thiếu %s VND.\n", money(hd.tongThanhToan - khach));
    }
    hd.khachDua = khach; hd.tienThoi = khach - hd.tongThanhToan;

    /* ---- Trừ tồn kho trong giao dịch; lỗi ghi file thì hoàn tác ---- */
    for (int i = 0; i < hd.soDong; i++) k->ds[kho_tim(k, hd.ct[i].ma)].tonKho -= hd.ct[i].soLuong;
    if (kho_luu(k) != 0) {
        for (int i = 0; i < hd.soDong; i++) k->ds[kho_tim(k, hd.ct[i].ma)].tonKho += hd.ct[i].soLuong;
        printf("  ! Không lưu được tồn kho — GIAO DỊCH BỊ HỦY, tồn kho giữ nguyên.\n");
        hd_giaiPhong(&hd); return;
    }
    (*soHDKe)++;
    if (log_ghi(&hd) != 0) printf("  ! Cảnh báo: chưa ghi được doanh thu ca vào %s\n", FILE_LOG);
    printf("\n"); hd_in(&hd, stdout);
    hd_luuFile(&hd);
    hd_giaiPhong(&hd);
}

static void nhapHang(KhoHang *k) {
    char ma[MAX_MA + 8]; MatHang m; memset(&m, 0, sizeof m);
    if (!docDong("\nMã hàng (nhập mới hoặc nhập thêm): ", ma, sizeof ma) || !ma[0]) return;
    chuanHoaMa(ma); ma[MAX_MA - 1] = 0;
    long long v;
    int idx = kho_tim(k, ma);
    if (idx >= 0) {
        printf("  Đã có: %s (tồn %d)\n", k->ds[idx].ten, k->ds[idx].tonKho);
        if (!docSo("  Số lượng nhập thêm: ", 1, 1000000, &v)) return;
        k->ds[idx].tonKho += (int)v;
    } else {
        memcpy(m.ma, ma, MAX_MA); m.ma[MAX_MA - 1] = 0;
        do { if (!docDong("  Tên hàng: ", m.ten, sizeof m.ten)) return; } while (!m.ten[0]);
        lamSachTen(m.ten);
        if (!docSo("  Đơn giá (chưa VAT): ", 0, 1000000000LL, &v)) return;
        m.donGia = v;
        if (!docSo("  Tồn kho ban đầu: ", 0, 1000000, &v)) return;
        m.tonKho = (int)v;
        if (!docSo("  Thuế VAT % (0/5/8/10): ", 0, 100, &v)) return;
        m.vat = (int)v;
        if (kho_them(k, &m) != 0) { printf("  ! Không thêm được.\n"); return; }
    }
    printf(kho_luu(k) == 0 ? "  Đã cập nhật kho.\n" : "  ! Lỗi lưu file kho.\n");
}

static void thongKeCa(void) {
    char ngay[32], hienTai[16]; time_t now = time(NULL);
    strftime(hienTai, sizeof hienTai, "%Y-%m-%d", localtime(&now));
    char nhap[32];
    docDong("\nNgày thống kê (YYYY-MM-DD, ENTER = hôm nay): ", nhap, sizeof nhap);
    snprintf(ngay, sizeof ngay, "%s", nhap[0] ? nhap : hienTai);

    FILE *f = fopen(FILE_LOG, "r");
    if (!f) { printf("  Chưa có dữ liệu doanh thu.\n"); return; }
    int soHD[4] = {0}; Tien tien[4] = {0}, vat[4] = {0}, tong[4] = {0};
    char dong[256];
    while (fgets(dong, sizeof dong, f)) {
        int so, ca; char d[16], t[16], nv[MAX_NV]; Tien a, b, c;
        if (sscanf(dong, "%d|%10[^|]|%8[^|]|%d|%31[^|]|%lld|%lld|%lld", &so, d, t, &ca, nv, &a, &b, &c) != 8) continue;
        if (ca < 1 || ca > 3 || strcmp(d, ngay) != 0) continue;
        soHD[ca]++; tien[ca] += a; vat[ca] += b; tong[ca] += c;
    }
    fclose(f);

    printf("\nTHỐNG KÊ DOANH THU NGÀY %s\n", ngay);
    gach(80);
    padR(stdout, "Ca làm việc", 22); padL(stdout, "Số HĐ", 7); padL(stdout, "Tiền hàng", 16); padL(stdout, "VAT", 14); padL(stdout, "Doanh thu", 16); putchar('\n');
    gach(80);
    int th = 0; Tien tt = 0, tv = 0, tc = 0;
    for (int c = 1; c <= 3; c++) {
        char b[16]; snprintf(b, sizeof b, "%d", soHD[c]);
        padR(stdout, TEN_CA[c], 22); padL(stdout, b, 7);
        padL(stdout, money(tien[c]), 16); padL(stdout, money(vat[c]), 14); padL(stdout, money(tong[c]), 16); putchar('\n');
        th += soHD[c]; tt += tien[c]; tv += vat[c]; tc += tong[c];
    }
    gach(80);
    char b[16]; snprintf(b, sizeof b, "%d", th);
    padR(stdout, "TỔNG NGÀY", 22); padL(stdout, b, 7);
    padL(stdout, money(tt), 16); padL(stdout, money(tv), 14); padL(stdout, money(tc), 16); putchar('\n');
    if (th) printf("Giá trị hóa đơn trung bình: %s VND\n", money(tc / th));
}

/* ======================= MAIN ======================= */
int main(void) {
#ifdef _WIN32
    system("chcp 65001 > nul");
#endif
    KhoHang kho = {0};
    kho_doc(&kho);
    qsort(kho.ds, (size_t)kho.n, sizeof *kho.ds, cmpHang);   /* đảm bảo thứ tự cho binary search */
    int soHD = log_soHDLonNhat();

    printf("======================================\n  HỆ THỐNG POS SIÊU THỊ MINI\n======================================\n");
    CaLamViec ca; moCa(&ca);

    for (;;) {
        printf("\n=========== MENU (%s - %s) ===========\n", TEN_CA[ca.ca], ca.nhanVien);
        printf("1. Bán hàng (quét mã)\n2. Xem danh mục & tồn kho\n3. Nhập hàng / thêm mặt hàng\n"
               "4. Thống kê doanh thu theo ca\n5. Đổi ca / thu ngân\n0. Thoát\n");
        long long c;
        if (!docSo("Chọn: ", 0, 5, &c)) break;
        switch (c) {
            case 1: banHang(&kho, &ca, &soHD); break;
            case 2: inDanhMuc(&kho); break;
            case 3: nhapHang(&kho); break;
            case 4: thongKeCa(); break;
            case 5: moCa(&ca); break;
            case 0: free(kho.ds); printf("Tạm biệt!\n"); return 0;
        }
    }
    free(kho.ds);
    return 0;
}
