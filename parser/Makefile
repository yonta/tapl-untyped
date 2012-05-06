all : Parser.grm.sig Parser.grm.sml lib

Parser.grm.sml : Parser.grm
	mlyacc Parser.grm

Parser.grm.sig : Parser.grm
	mlyacc Parser.grm

lib :

