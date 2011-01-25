# Fast base16 support

This package provides a Haskell library for working with base16-encoded
data quickly and efficiently, using the ByteString type.


# Performance

This library is written in pure Haskell, and it's fast:

* 250 MB/sec encoding

* 200 MB/sec strict decoding (per RFC 4648)

* 100 MB/sec lenient decoding


# Get involved!

Please report bugs via the
[bitbucket issue tracker](http://github.com/mailrank/base16-bytestring).

Master [github repository](http://github.com/mailrank/base16-bytestring):

* `git clone git://github.com/mailrank/base16-bytestring.git`

There's also a [Mercurial mirror](http://bitbucket.org/bos/base16-bytestring):

* `hg clone http://bitbucket.org/bos/base16-bytestring`

(You can create and contribute changes using either git or Mercurial.)


# Authors

This library is written and maintained by Bryan O'Sullivan,
<bos@mailrank.com>.
