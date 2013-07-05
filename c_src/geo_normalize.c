#include <stdlib.h>
#include <string.h>
#include <iconv.h>
#include <errno.h>

static char *geo_charmap[] = {
	NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL ,
	NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL ,
	NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL ,
	"0"  , "1"  , "2"  , "3"  , "4"  , "5"  , "6"  , "7"  , "8"  , "9"  , NULL , NULL , NULL , NULL , NULL , NULL ,
	NULL , "a"  , "b"  , "c"  , "d"  , "e"  , "f"  , "g"  , "h"  , "i"  , "j"  , "k"  , "l"  , "m"  , "n"  , "o"  ,
	"p"  , "q"  , "r"  , "s"  , "t"  , "u"  , "v"  , "w"  , "x"  , "y"  , "z"  , NULL , NULL , NULL , NULL , NULL ,
	NULL , "a"  , "b"  , "c"  , "d"  , "e"  , "f"  , "g"  , "h"  , "i"  , "j"  , "k"  , "l"  , "m"  , "n"  , "o"  ,
	"p"  , "q"  , "r"  , "s"  , "t"  , "u"  , "v"  , "w"  , "x"  , "y"  , "z"  , NULL , NULL , NULL , NULL , NULL ,
	NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL ,
	NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL ,
	NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL ,
	NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL , NULL ,
	"a"  , "a"  , "a"  , "a"  , "a"  , "a"  , "ae" , "c"  , "e"  , "e"  , "e"  , "e"  , "i"  , "i"  , "i"  , "i"  ,
	"d"  , "n"  , "o"  , "o"  , "o"  , "o"  , "o"  , "x"  , "o"  , "u"  , "u"  , "u"  , "u"  , "y"  , "th" , "ss" ,
	"a"  , "a"  , "a"  , "a"  , "a"  , "a"  , "ae" , "c"  , "e"  , "e"  , "e"  , "e"  , "i"  , "i"  , "i"  , "i"  ,
	"o"  , "n"  , "o"  , "o"  , "o"  , "o"  , "o"  , NULL , "o"  , "u"  , "u"  , "u"  , "u"  , "y"  , "th" , "y"  ,
	};

/*
 * This takes a UTF-8 string like "Rivière-du-Loup" and, optionally,
 * a pre-allocated UTF-8 to ISO-8859-1//IGNORE conversion descriptor,
 * and returns a normalized ascii version "riviereduloup"
 *
 * It's indended for processing maxmind geo data as well as geo-targetting rules before
 * doing strcmp(), to tolerate variations in accent/whitespace/punctuation/capitalization
 *
 * Returns a freshly-malloc()ed string on success, NULL on failure.  Be sure to free()
 * result when you no longer need it
 */
char *
geo_normalize(char *in, iconv_t cd)
{
	iconv_t local_cd = NULL;
	char *inptr, *out, *outptr, *latin, *latinptr;
	char *c, *r;
	size_t inlen, latinlen, conv;

	if (in == NULL)
		return NULL;
	inlen = strlen(in);

	if (cd == NULL) {
		local_cd = iconv_open("ISO-8859-1//IGNORE", "UTF-8");
		if (local_cd == (iconv_t) -1)
			return NULL;
		cd = local_cd;
	}

	// Do a single allocation to be used for:
	// [normalized output]\x00[iconv output]\x00
	out = malloc((inlen*4)+2);
	if (out == NULL) {
		if (local_cd != NULL)
			iconv_close(local_cd);
		return NULL;
	}
	latin = out + (inlen*2) + 1;
	latinlen = (inlen*2) + 1;

	inptr = in;
	latinptr = latin;
	conv = iconv(cd, &inptr, &inlen, &latinptr, &latinlen);
	if (local_cd != NULL)
		iconv_close(local_cd);

	if (conv == (size_t) -1) {
		// Failed
		free(out);
		return NULL;
	}

	// Scan latin and normalize into out
	outptr = out;
	for (c = latin; c < latinptr; c++) {
		if ((r = geo_charmap[(unsigned char)*c]) != NULL) {
			while (*r != 0) {
				*outptr = *r;
				r++;
				outptr++;
			}
		}
	}
	*outptr = 0;

	return out;
}
