# epdf
An incomplete library to make PDF files from emacs.

This library took me many months to build, but I am not going to finish it. 

It allows to build PDF files with embedded fonts. 

And it has a function to export a buffer to a PDF. It doesn't allow for text 
properties, only black text.
What it does do is find the right font for each char, and shape it properly.

The hello.pdf file is an example of an exported, modified HELLO buffer from emacs.

I think the concept works, and it would make for a great tool inside emacs.

It works with two complicated file formats, PDF and TTF, but shows how they 
can be dealt with in pure lisp, with no non-emacs dependencies.

The most difficult thing is preparing the fonts so PDF can use them. 
The information needs to be heavily preprocessed, and then the 
font needs to be subsetted end embedded. 

In retrospective, I probably would have taken the code from cairo.
