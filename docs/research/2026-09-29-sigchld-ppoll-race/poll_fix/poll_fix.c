#define _GNU_SOURCE

#include <errno.h>
#include <poll.h>
#include <pthread.h>
#include <signal.h>

#include <caml/bigarray.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <caml/signals.h>

#define HAS_PPOLL 1

/* only defined in the runtime with CAML_INTERNALS */
CAMLextern int caml_convert_signal_number (int);
/* Diagnostic repair for OCaml 5: this is a runtime-internal API. */
CAMLextern int caml_check_pending_signals (void);


#ifdef HAS_PPOLL
static void
decode_sigset(value vset, sigset_t * set)
{
	sigemptyset(set);
	for (/*nothing*/; vset != Val_emptylist; vset = Field(vset, 1)) {
		int sig = caml_convert_signal_number(Int_val(Field(vset, 0)));
		sigaddset(set, sig);
	}
}
#endif

#define S_IN_NS 1000000000LL
value
__wrap_caml_iomux_ppoll(value v_fds, value v_nfds, value v_timo, value v_sigmask)
{
#ifdef HAS_PPOLL
	CAMLparam4(v_fds, v_nfds, v_timo, v_sigmask);
	struct pollfd *fds;
	struct timespec *timo;
	struct timespec ts;
	sigset_t *psigmask, sigmask;
	nfds_t nfds;
	int64_t timo64;
	int r;

	fds = Caml_ba_data_val(v_fds);
	nfds = Int_val(v_nfds);
	timo64 = Int64_val(v_timo);
	if (timo64 == -1LL)
		timo = NULL;
	else {
		ts.tv_sec = (time_t)(timo64 / S_IN_NS);
		ts.tv_nsec = (time_t)(timo64 % S_IN_NS);
		timo = &ts;
	}

	if (v_sigmask == Val_emptylist)
		psigmask = NULL;
	else {
		decode_sigset(v_sigmask, &sigmask);
		psigmask = &sigmask;
	}

	/* Close the gap between the runtime pending-signal check and ppoll.
	 * A signal delivered in that gap otherwise remains pending in OCaml
	 * while ppoll sleeps forever. Block delivery here, then atomically
	 * restore the requested mask in ppoll. Already-recorded signals must
	 * return to OCaml rather than enter a blocking syscall. */
	sigset_t all_signals, oldmask;
	sigfillset(&all_signals);
	int mask_error = pthread_sigmask(SIG_BLOCK, &all_signals, &oldmask);
	if (mask_error != 0) {
		errno = mask_error;
		uerror("pthread_sigmask", Nothing);
	}
	/* No OCaml callbacks may raise while the temporary mask is installed. */
	caml_enter_blocking_section_no_pending();
	if (caml_check_pending_signals()) {
		r = -1;
		errno = EINTR;
	} else {
		r = ppoll(fds, nfds, timo, psigmask ? psigmask : &oldmask);
	}
	int saved_errno = errno;
	mask_error = pthread_sigmask(SIG_SETMASK, &oldmask, NULL);
	errno = mask_error != 0 ? mask_error : saved_errno;
	if (mask_error != 0) r = -1;
	caml_leave_blocking_section();
	if (r == -1) /* this allocs */
		uerror("ppoll", Nothing);

	CAMLreturn(Val_int(r));
#else /* HAS_PPOLL */
	errno = ENOSYS;
	uerror("ppoll", Nothing);
#endif /* HAS_PPOLL */
}
#undef S_IN_NS
