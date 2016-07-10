
#include <assert.h>
#include <string.h>

#include <netinet/in.h>
#include <arpa/inet.h>

#include <pthread.h>

#include <libmilter/mfapi.h>

#include <stdio.h>

int mw_connect(SMFICTX *ctx, char *, struct sockaddr *);
int mw_helo(SMFICTX *ctx, char *);
int mw_envfrom(SMFICTX *ctx, char **);
int mw_envrcpt(SMFICTX *ctx, char **);
int mw_header(SMFICTX *ctx, char *, char *);
int mw_eoh(SMFICTX *ctx);
int mw_body(SMFICTX *ctx, unsigned char *, size_t);
int mw_eom(SMFICTX *ctx);
int mw_abort(SMFICTX *ctx);
int mw_close(SMFICTX *ctx);

int mw_main();

/*
	SMFIF_ADDHDRS
	SMFIF_CHGHDRS
	SMFIF_CHGBODY
	SMFIF_ADDRCPT
	SMFIF_DELRCPT
*/

static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

int c = 0;

void lock()
{
#ifdef LOCK
	int r = pthread_mutex_lock(&mutex);
	if (r != 0)
		printf("mutex lock error: %d\n", r);
	c++;
	assert(c == 1);
#endif
}

void unlock()
{
#ifdef LOCK
	int r;
	assert(c == 1);
	c--;
	r = pthread_mutex_unlock(&mutex);
	if (r != 0)
		printf("mutex unlock error: %d\n", r);
#endif
}

int wrap_mw_connect(SMFICTX *ctx, char *h, struct sockaddr *a)
{
	int r;
	lock();
	r = mw_connect(ctx, h, a);
	unlock();
	return r;
}
int wrap_mw_helo(SMFICTX *ctx, char *m)
{
	int r;
	lock();
	r = mw_helo(ctx, m);
	unlock();
	return r;
}
int wrap_mw_envfrom(SMFICTX *ctx, char **f)
{
	int r;
	lock();
	r = mw_envfrom(ctx, f);
	unlock();
	return r;
}
int wrap_mw_envrcpt(SMFICTX *ctx, char **rc)
{
	int r;
	lock();
	r = mw_envrcpt(ctx, rc);
	unlock();
	return r;
}
int wrap_mw_header(SMFICTX *ctx, char *a, char *b)
{
	int r;
	lock();
	r = mw_header(ctx, a, b);
	unlock();
	return r;
}
int wrap_mw_eoh(SMFICTX *ctx)
{
	int r;
	lock();
	r = mw_eoh(ctx);
	unlock();
	return r;
}
int wrap_mw_body(SMFICTX *ctx, unsigned char *b, size_t l)
{
	int r;
	lock();
	r = mw_body(ctx, b, l);
	unlock();
	return r;
}
int wrap_mw_eom(SMFICTX *ctx)
{
	int r;
	lock();
	r = mw_eom(ctx);
	unlock();
	return r;
}
int wrap_mw_abort(SMFICTX *ctx)
{
	int r;
	lock();
	r = mw_abort(ctx);
	unlock();
	return r;
}
int wrap_mw_close(SMFICTX *ctx)
{
	int r;
	lock();
	r = mw_close(ctx);
	unlock();
	return r;
}

struct smfiDesc milter_filter = {
	"hs_filter",
	SMFI_VERSION,
	SMFIF_ADDHDRS | SMFIF_CHGHDRS | SMFIF_ADDRCPT | SMFIF_DELRCPT,
	wrap_mw_connect,
	wrap_mw_helo,
	wrap_mw_envfrom,
	wrap_mw_envrcpt,
	wrap_mw_header,
	wrap_mw_eoh,
	wrap_mw_body,
	wrap_mw_eom,
	wrap_mw_abort,
	wrap_mw_close
};

void addr_of_sockaddr(char *buf, int n, struct sockaddr_in *sa)
{
	inet_ntop(AF_INET, &sa->sin_addr.s_addr, buf, n);
}

int mw_run_filter(const char *name, const char *sock)
{
	int r = 0;
	smfi_setconn((char*)sock);
	if (smfi_register(milter_filter) == MI_FAILURE) {
		return -1;
	}
	r = smfi_main();
	return r;
}


