# Collectd Haskell

Haskell bindings for [Collectd](https://github.com/collectd/collectd).

# Building

This plugin consists of 2 plugins (in spirit of Java, Python and Perl bindings).
First one - is a binding itself, that registers Haskell runtime and lets
you load Haskell plugins.

Second one - plugin itself. Whenever we're close to alpha release, there will be
a Cabal package and reasonable distribution. For now, you have to pretty
much build everything right form that project.

# Building

For now, you have to run two tasks:

```
make install_bindings
make install_plugin
```

# Pitfalls

  * If your GHC version is anything but 7.8.3, it won't build. For now, even GHC
    version is hardcoded. You'd have to fix Makefile for youself (or send
    a patch with a configuration option)
  * If your collectd is located anywhere but `~/opt/collectd`, you'd have to pass
    a `COLLECTD_LIB` option to make.

# License

Licensed under the MIT License.

Copyright (c) 2014 Oleksandr Petrov
