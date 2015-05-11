#! /bin/sh


# the & for backgrounding works in bash--does it work in other sh variants?

${RPROG:-R} --vanilla <<EOF > ${OUT:-/dev/null} 2>&1 &

requireNamespace(strmr)
options(timeout=getOption("strmr.timeout", 5L))
strmr:::.strmr_loop(snow::makeSOCKmaster())

EOF
