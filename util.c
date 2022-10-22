/* See LICENSE.dwm file for copyright and license details. */
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "util.h"

void *
ecalloc(size_t nmemb, size_t size)
{
	void *p;

	if (!(p = calloc(nmemb, size)))
		die("calloc:");
	return p;
}

void
die(const char *fmt, ...) {
	va_list ap;

	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);

	if (fmt[0] && fmt[strlen(fmt)-1] == ':') {
		fputc(' ', stderr);
		perror(NULL);
	} else {
		fputc('\n', stderr);
	}

	exit(1);
}

void
_send_log(const char *arg, ...) {
	va_list ap;
    SCM scm =scm_make_list(scm_from_int(0), SCM_UNSPECIFIED);
    char *para;
    char *para2;

	va_start(ap, arg);
    scm=scm_cons(REF("gwwm utils srfi-215",arg) ,scm);
    scm=scm_cons(scm_from_utf8_string(va_arg(ap, char *)),scm);
    while(1) {
      para=va_arg(ap, char *);
      if ( strcmp( para, "/0") == 0 )
        break;
      para2=va_arg(ap, char *);
      scm=scm_cons2(scm_from_utf8_string(para2),
                    scm_from_utf8_symbol(para),
                    scm);
    }
	va_end(ap);
    scm_apply_0(REF("gwwm utils srfi-215","send-log"), scm_reverse(scm)) ;
}
