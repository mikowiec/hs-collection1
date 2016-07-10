
#include <png.h>
#include <stdio.h>
#include <stdlib.h>


#include "hs_png.h"



/****************************************************************************/

static void png_cexcept_error(png_structp png_ptr, png_const_charp msg)
{
	fprintf(stderr, "png loading error: %s\n", msg);
}

struct png {
	png_uint_32 width, height;
	png_byte *data;
};

static void load_png_file(const char *name, struct png *png)
{
	png_byte sig[8];
	int i, bit_depth, color_type;
	double gamma;
	png_uint_32 channels;
	png_uint_32 row_bytes;
	png_byte **row_pointers = 0;
	png_structp png_ptr = 0;
	png_infop info_ptr = 0;

	FILE *fp;

	if (!(fp = fopen(name, "rb"))) {
		fprintf(stderr, "png open error (%s)\n", name);
		exit(1);
	}

	fread(sig, 1, 8, fp);
	if (!png_check_sig(sig, 8)) {
		fprintf(stderr, "png signature error | ");
		for (i = 0; i < 8; ++i) 
			fprintf(stderr, "%02x ", sig[i]);
		fprintf(stderr, " |\n");
		exit(1);
	}

	png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, 0, png_cexcept_error, 0);
	if (!png_ptr) {
		fprintf(stderr, "png read error\n");
		exit(1);
	}

	info_ptr = png_create_info_struct(png_ptr);
	if (!info_ptr)
	{
		png_destroy_read_struct(&png_ptr, 0, 0);
		fprintf(stderr, "png read error\n");
		exit(1);
	}

	png_init_io(png_ptr, fp);

	png_set_sig_bytes(png_ptr, 8);
	png_read_info(png_ptr, info_ptr);
	png_get_IHDR(png_ptr, info_ptr, &png->width, &png->height,
		                        &bit_depth, &color_type, 0, 0, 0);

	if (bit_depth == 16)
		png_set_strip_16(png_ptr);
	if (color_type == PNG_COLOR_TYPE_PALETTE || bit_depth < 8)
		png_set_expand(png_ptr);
	if (png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS))
		png_set_expand(png_ptr);
	if (color_type == PNG_COLOR_TYPE_GRAY || color_type == PNG_COLOR_TYPE_GRAY_ALPHA)
		png_set_gray_to_rgb(png_ptr);
	if (bit_depth == 8 && color_type == PNG_COLOR_TYPE_RGB)
		png_set_filler(png_ptr, 255, PNG_FILLER_AFTER);
	if (png_get_gAMA(png_ptr, info_ptr, &gamma))
		png_set_gamma(png_ptr, 2.2, gamma);

	png_read_update_info(png_ptr, info_ptr);

	png_get_IHDR(png_ptr, info_ptr, &png->width, &png->height,
		                        &bit_depth, &color_type, 0, 0, 0);

	row_bytes = png_get_rowbytes(png_ptr, info_ptr);
	channels = png_get_channels(png_ptr, info_ptr);

	if ((png->data = (png_byte *) malloc(row_bytes * png->height * sizeof(png_byte))) == 0) {
		fprintf(stderr, "png malloc error\n");
		exit(1);
	}

	if ((row_pointers = (png_bytepp) malloc((png->height) * sizeof(png_bytep))) == 0) {
		fprintf(stderr, "png malloc error\n");
		exit(1);
	}

	for (i = 0; i < png->height; i++)
		row_pointers[i] = png->data + i * row_bytes;

	png_read_image(png_ptr, row_pointers);
	png_read_end(png_ptr, NULL);
	free(row_pointers);
}

struct png *hs_png_load(const char *file)
{
	struct png *png;
	png = (struct png *)malloc(sizeof(struct png));
	load_png_file(file, png);
	return png;
}

void hs_png_free(struct png *png)
{
	free(png->data);
	free(png);
}

int hs_png_get_width(struct png *png)
{
	return png->width;
}

int hs_png_get_height(struct png *png)
{
	return png->height;
}

void *hs_png_get_data(struct png *png)
{
	return (void*)png->data;
}

#ifdef TEST_PNG
int main()
{
	int x, y;
	struct png *png = hs_png_load("main.png");
	printf("png: %d %d\n", hs_png_get_width(png), hs_png_get_height(png));
	for (x = 0; x < 100; x+=10) {
		for (y = 0; y < 100; y+=10) {
			png_byte *p = png->data + (y*png->width + x) * 4;
			printf("-> %d %d %d %d\n", p[0], p[1], p[2], p[3]);
		} 
	}
	hs_png_free(png);
	return 0;
}
#endif

