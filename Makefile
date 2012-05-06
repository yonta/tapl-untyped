all : Absyn.ppg.sml load.sml parser

Absyn.ppg.sml : Absyn.ppg
	smlformat Absyn.ppg

parser :
	cd parser && $(MAKE)
