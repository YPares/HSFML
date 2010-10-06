#setupPkgDB()
#{
#    ghc-pkg init pkg.conf.d
#}

configure()
{
    cabal configure #--package-db pkg.conf.d
}

build()
{
    cabal build
    #cabal register --inplace
}

demobuild()
{
    ghc --make -threaded "$@" \
        -package-conf dist/package.conf.inplace \
        -odir demo/build \
        -hidir demo/build \
        -o demo/demo \
        demo/src/demo.hs
}

demo()
(
    cd demo &&
    ./demo "$@"
)

democlean()
{
    \rm -rf demo/build
    \rm -f demo/demo
    \rm -f demo/demoC
}

demoCbuild()
{
    mkdir demo/build &&
    gcc -c demo/src/demoC.c -o demo/build/demoC.o &&
    gcc demo/build/demoC.o -o demo/demoC \
        -lcsfml-system -lcsfml-window -lcsfml-graphics
}

demoC()
(
    cd demo &&
    ./demoC "$@"
)

