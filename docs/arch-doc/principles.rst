.. index:: design principles

Design Principles
-----------------

This section provides a brief outline of some of our requirements and the
general approaches we intend to use in order to meet them. Some of the  specific
libraries and tools mentioned may change as the design of the framework evolves.


**Flexibility**
    - Component Architecture (Zope's or similar)
        - Adaptor design pattern
    - Isolated Python Environments using zc.buildout
    - Support multiple IPC protocols by using Twisted_
    
.. _Twisted : http://twistedmatrix.com/trac

**Ease of extensibility**
    - Component Publishing

**Portability**
    - use of Python/wxPython

**100% open source infrastructure**
    - Python's many open source libraries

**Location transparency**
    - Abstract Factory design pattern
    - proxy objects


