# table

Formats lines of TSV, CSV, or DSV (delimiter-separated values) into a pretty
plain text table, wrappings cells with long content to try to fit the table in
the screen.

## Usage 

```
table

Usage: table ([-d DELIM] | [-s]) [-r] [-w WIDTH]
  Pretty format TSV input into table with aligned and wrapped cells

Available options:
  -h,--help                Show this help text
  -d DELIM                 Input field delimiter. Default is TAB (\t).
  -s                       Use any run of whitespace as input field delimiter
  -r                       Don't print row dividers
  -w WIDTH                 Max table width. Defaults to value of `tput cols`
                           command.

https://github.com/danchoi/table
```


