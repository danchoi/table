# table

Formats lines of TSV, CSV, or DSV (delimiter-separated values) into a pretty
plain text table, wrappings cells with long content to try to fit the table in
the screen.


## Install

Assuming you have a recent version of the [Haskell
platform](https://www.haskell.org/platform/) on your system, 

    cabal update
    cabal install table

## Usage 

```
table

Usage: table ([-d DELIM] | [-s]) [-c] [-R] [-H] [-w WIDTH]
  Pretty format TSV input into table with aligned and wrapped cells

Available options:
  -h,--help                Show this help text
  -d DELIM                 Input field delimiter. Default is TAB (\t).
  -s                       Use any run of whitespace as input field delimiter
  -c                       Parse input as CSV
  -R                       Don't print row dividers
  -H                       Print header row divider
  -w WIDTH                 Max table width. Defaults to value of `tput cols`
                           command.

https://github.com/danchoi/table
```

## Example

Format Rails issues from the GitHub API into a table. This example uses
[jq](http://stedolan.github.io/jq/) and
[jsontsv](https://github.com/danchoi/jsontsv) to transform the GitHub API JSON
into tab separated values before piping that data into `table`:

```bash
curl -s "https://api.github.com/repos/rails/rails/issues" | 
jq -M '.[]' | 
jsontsv -H 'number title user.login:user state' |
table
```

Outputs this when the terminal width is 72 characters: 

```
--------+--------------------------------------+----------------+-------
 number | title                                | user           | state 
--------+--------------------------------------+----------------+-------
  18290 | Updating guides for Rails 5.0        | lucascaton     | open  
--------+--------------------------------------+----------------+-------
  18288 | Add --skip-action-mailer (or -M) to  | claudiob       | open  
        | rails generate                       |                |       
--------+--------------------------------------+----------------+-------
  18287 | Rails 4.2.0: Scaffold generator      | dldinternet    | open  
        | with --helper=false produced an      |                |       
        | error                                |                |       
--------+--------------------------------------+----------------+-------
  18285 | undefined method 'clear' for         | nPn-           | open  
        | nil:NilClass in                      |                |       
        | actionpack-4.2.0/lib/action_controll |                |       
        | er/test_case.rb                      |                |       
--------+--------------------------------------+----------------+-------
  18283 | Prefer `array?` rather than `array`  | kamipo         | open  
--------+--------------------------------------+----------------+-------
  18279 | Refactoring of add_constraints in    | eileencodes    | open  
        | AssociationScope                     |                |       
--------+--------------------------------------+----------------+-------
  18273 | Rails 3.2.21 (4.x too?):             | jensb          | open  
        | attribute_will_change! with symbol   |                |       
        | parameter causes "multiple           |                |       
        | assignment" error in Postgres        |                |       
--------+--------------------------------------+----------------+-------
  18257 | ActionView::Helpers::TranslationHelp | jcoyne         | open  
        | er.translate makes unsafe values     |                |       
        | html_safe                            |                |       
...
```

You can use `table` to pretty-format TSV output from `mysql`, `psql`, or
`sqlite3` as well, especially when you need row cell content to wrap to fit
everything nicely on the screen.

## Author

* Daniel Choi <https://github.com/danchoi>

## License

MIT License
