
#ifndef HS_PNG_H
#define HS_PNG_H

#ifdef __cplusplus
extern "C" {
#endif

struct png;

struct png *hs_png_load(const char *file);
void hs_png_free(struct png *ptr);
int hs_png_get_width(struct png *ptr);
int hs_png_get_height(struct png *ptr);
void *hs_png_get_data(struct png *ptr);


#ifdef __cplusplus
}
#endif

#endif

