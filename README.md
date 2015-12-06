# img2ascii
Image to ASCII-Converter

# build instructions

Execute `stack build`. Result is then in your `.stack-work/dist/$system/$cabal/build/img2ascii/` folder. Replace `$system` and `$cabal` according to your installation.

Alternatively you can install the program with `stack install` to `~/.local/img2ascii` or for all users (if `stack install` is invoked as root).

Three true-color example images in the resolution of 225x65, 225x65 and 100x40 are included. To view them just type `cat image1.img` in a suitable (aka TrueColor) terminal.

One 256-color example is also included. Type `cat rose_256.img` to view that.

# example

![rose-image](https://raw.githubusercontent.com/Drezil/img2ascii/master/example_small.png)
