SOURCES = tools.ml \
          gtkHelper.ml gtkWaveDisplay.mli gtkWaveDisplay.ml ias.ml \
          soundEditor.ml soaplab.ml
RESULT = soaplab
PACKS = lablgtk2.auto-init lablgtk2.glade ogg vorbis portaudio bigarray
THREADS = yes

all: ias.ml nc

test: all
	./soaplab lin.ogg

ias.ml: ias.glade
	lablgladecc2 $< > $@

include OCamlMakefile
