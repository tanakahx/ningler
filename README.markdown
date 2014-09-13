# ningler

Ningler is a blog application sample using [ningle](https://github.com/fukamachi/ningle), a lightweight web application framework for Common Lisp. Ningler is a Common Lisp version of Flaskr, which appears in the tutorial documentation of Flask, a Python based microframework.

## Usage

Database file must be created first time.

``` cl
(init-db)
```

The file path of the database is assigned to the variable `*db*` in ningler.lisp. You can change the value of this variable as you want.

## Installation

Clone this repository into the local-projects of quicklisp,

``` console
$ cd ~/quicklisp/local-projects/site
$ git clone git@github.com:tanakahx/ningler.git
```

and then load it with quickload.

``` cl
(ql:quickload "ningler")
```

You can access the site url `http://localhost:5000` with a web browser.