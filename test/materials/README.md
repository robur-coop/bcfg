scfg Test Suite
===============

This directory contains tests for the scfg configuration format.

All tests in the `invalid` directory are expected to fail parsing, while
all tests in the `valid` directory are expected to parse successfully.

A parser which is being tested should read each file in the `valid` directory
and produce output identical to the corresponding file in the `expected`
directory and reject all files in the `invalid` directory.

The following transformation rules should be applied for generating the expected
output from the valid input.


Transformation Rules
--------------------

The files in the `expected` directory are not identical to the input files in
the `valid` directory. The following transformations are applied:

* All comments are removed.
* Extraneous whitespace (spaces, tabs, newlines) is removed
* All whitespace is normalized to a single space character.
* All names and parameters must be enclosed in double quotes.
* All unnecessary escapes are removed from names and parameters.
* Opening braces `{` remain on the same line as their associated directive.
* Closing braces `}` are on their own line, indented to match the indentation
  level of their opening directive
* All directives of a block must be indented by one tabulator character `x09` 
  per level of nesting.


For example, the following input file:

	# This is a comment
	server	 {
		listen	80
		server_name    example.com	 www.example.com

		location / {
			root   /var/www/html
			index  index.html index.htm
		}
	}

Would produce the following expected output:

	"server" {
		"listen" "80"
		"server_name" "example.com" "www.example.com"
		"location" "/" {
			"root" "/var/www/html"
			"index" "index.html" "index.htm"
		}
	}

