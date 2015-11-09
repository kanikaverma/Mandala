.PHONY: clean
clean:
	find . -name '*.cmo' -delete
	find . -name '*.cmi' -delete
	find . -name '*.proc*' -delete
	find . -name 'scanner.ml' -delete
	find . -name 'parser.ml' -delete
	find . -name 'parser.mli' -delete	