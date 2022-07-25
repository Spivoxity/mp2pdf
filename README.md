# mp2pdf
Translator from MetaPost output to TeX input, suitable for pdfTeX.

Typical usage:

mp2pdf figs.1 >figs-1.mtx

followed by \input figs-1.mtx embedded in a TeX document.

The same function is performed by some TeX macros by Hans Hagen, but I
prefer this way: it has practically limitless capacity and perfect
precision, and avoids some problems with ligatures.
