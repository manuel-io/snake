CCLD := ghc
TARGET = bin/snake
ARGS := -O2 -threaded

$(TARGET): Main.hs
	mkdir -p "$$(dirname $(TARGET))"
	$(CCLD) -o $(TARGET) $(ARGS) $<

.PHONY: clean

clean:
	rm -f *.o *.hi
	rm -f $(TARGET)
	rm -rf dist
	rmdir "$$(dirname $(TARGET))"
