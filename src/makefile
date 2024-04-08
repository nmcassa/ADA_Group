F = -lgtkada -lgtkada_gtk -lgtk-3 -lgdk-3 -lglib-2.0

all: main

main: main.adb
	gnatmake main.adb

clean:
	rm *.o
	rm *.ali
	rm main
	rm b~*
