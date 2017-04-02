;; instructions on how to load oct only, assuming you are in
;; lisp/generic directory.  This is RJF's version of OCT, based on
;; RTOY's program, based on Yozo Hida's QD program (in C).

;; Why is RTOY version and RJF's version not the same?  Well, parallel
;; but different tracks in some respects -- minor differences in
;; design philosophy, and minor differences in emphasis. RJF's version
;; assumes the possibility of generic arithmetic (GA) involving many
;; other kinds of arithmetic, rather than piling it on OCT. RJF's
;; version runs in Allegro CL and does not especially make an effort
;; to optimize for CMU-CL.  RTOY's version was written, debugged and
;; optimized for CMU-CL and ported to Allegro (etc) with RJF's help,
;; but doesn't seem to be debugged quite, for Allegro.  The advantage
;; of the CMU-CL version is that the quad-double number can be packed
;; differently: CMU-CL supports double-double, and so a complex
;; double-double is sufficient for a (real) quad.  Allegro, and most
;; other lisps use a 4-element array of doubles. CMU-CL also supports
;; inlining of program calls as a compiler optimization. Allegro does
;; not do this for user-defined programs.  CMU-CL takes a long time to
;; compile this code. Allegro does it virtually instantaneously.  One
;; of the consequences of aggressive optimization in Allegro is not
;; saving certain floating-point flags, and so the fastest
;; optimization setting should be (declare (optimize (speed 3)(safety
;; 1))), which is hardly slower than (safety 0).

;; RTOY has played more with elementary functions than RJF, though
;; RJF's may also work. RTOY has done more testing (on CMU-CL). RTOY's
;; i/o functions seem considerably more complicated, but have slightly
;; nicer properties pertaining to rounding. RTOY uses CLOS more
;; thoroughly.  RTOY has more complex number stuff. RTOY seems to have
;; though some about NaN and Infinities.  But it doesn't work on
;; Allegro :(



;; Anyhow, here's what to do to get RJF's stuff running on allegro:

(load "ga")		;generic arithmetic; loads packs, too
(load "octi")
(load "oct")

;; then do (in-package :oct)
;; or just :pa :oct
;; any of these files can be compiled, probably best done after loading
;; this file.


