# CL-HRAC [![Build Status](https://travis-ci.org/cl-rabbit/cl-hrac.svg)](https://travis-ci.org/cl-rabbit/cl-hrac) [![Coverage Status](https://coveralls.io/repos/cl-rabbit/cl-hrac/badge.svg?branch=master&service=github)](https://coveralls.io/github/cl-rabbit/cl-hrac?branch=master)
RabbitMQ Management HTTP API in Common Lisp

## Note
This lib depends on hot version of [ia-hash-table](https://github.com/deadtrickster/ia-hash-table)

## Examples

```lisp
(ia-hash-table:enable-ia-syntax)

(let ((hrac:*connection* (make-instance 'hrac:drakma-connection)))
  (log:info "RabbitMQ alive: ~a" (hrac:alive-p))
  (let ((overview (hrac:overview)))
    (log:info "RabbitMQ node ~a, version ~a" #Ioverview.node #Ioverview.rabbitmq_version))
  (let ((exchanges (hrac:exchanges)))
    (log:info "Already ~a exchanges declared" (length exchanges))
    (log:info "3d exchange name is ~a" #Iexchanges.[2].name)))

<INFO> [07:28:06] cl-user () - RabbitMQ node rabbit@office, version 3.6.0
<INFO> [07:28:06] cl-user () - Already 13 exchanges declared
<INFO> [07:28:06] cl-user () - 3d exchange name is amq.fanout
```

## License

```
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

By contributing to the project, you agree to the license and copyright terms therein and release your contribution under these terms.

## Copyright

Copyright (c) 2016 Ilya Khaprov <ilya.khaprov@publitechs.com> and [CONTRIBUTORS](CONTRIBUTORS.md)

CL-HRAC uses a shared copyright model: each contributor holds copyright over their contributions to CL-HRAC. The project versioning records all such contribution and copyright details.

If a contributor wants to further mark their specific copyright on a particular contribution, they should indicate their copyright solely in the commit message of the change when it is committed. Do not include copyright notices in files for this purpose.
