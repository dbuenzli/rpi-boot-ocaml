(* This file was auto generated with:

printf 'let img = (321, 280, "%s")\n' \
$(curl -L https://raw.githubusercontent.com/ocaml/ocaml-logo/master/Colour/PNG/colour-transparent-icon.png \
| convert png:- -geometry x280 -background black -alpha Remove bgr:- \
| hexdump -v -e '"\\\x" 1/1 "%02X"' )

*)
