How those .mli files were obtained
===================================

Commands that were used
-----------------------

First, opam-2.0.0~beta2 was installed.
Then:

    opam  switch create 4.04.0
    eval `opam env`
    opam list --installable | egrep -v '^#' | awk '{print $1}' \
      > opam_installable
    opam depext -i $(cat opam_installable)
    opam install --soft-request $(cat opam_installable)
