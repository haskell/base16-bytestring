# Fast base16 support [![Hackage version](https://img.shields.io/hackage/v/base16-bytestring.svg?label=Hackage)](https://hackage.haskell.org/package/base16-bytestring) [![Stackage version](https://www.stackage.org/package/base16-bytestring/badge/lts?label=Stackage)](https://www.stackage.org/package/base16-bytestring) [![Build Status](https://secure.travis-ci.org/haskell/base16-bytestring.svg?branch=master)](http://travis-ci.org/haskell/base16-bytestring)

This package provides a Haskell library for working with base16-encoded
data quickly and efficiently, using the ByteString type.


# Performance

This library is written in pure Haskell, and it's fast:

* 250 MB/sec encoding

* 200 MB/sec strict decoding (per RFC 4648)

* 100 MB/sec lenient decoding


# Get involved!

Please report bugs via the
[github issue tracker](http://github.com/haskell/base16-bytestring).

Master [github repository](http://github.com/haskell/base16-bytestring):

* `git clone git://github.com/haskell/base16-bytestring.git`


# Authors

This library is written and maintained by Bryan O'Sullivan,
<bos@serpentine.com>. It is maintained by Herbert Valerio Riedel,
<hvr@gnu.org> and Mikhail Glushenkov, <mikhail.glushenkov@gmail.com>.
