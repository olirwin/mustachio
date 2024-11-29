# mustachio

A simple JSON / Mustache template combo.

The command:

```bash
stack install
```

will install the executable to the path.

## Program usage

```bash
$ mustachio -h
Usage: mustachio [OPTIONS]
  -t FILE  --template=FILE  Path to the template file
  -d FILE  --data=FILE      Path to the data file (JSON)
  -o FILE  --output=FILE    Path to the output file
  -h       --help           Display this help message
  -v       --verbose        Enable verbose mode
```

## Error handling

File reading errors and syntax errors are handled differently.

## Testing

A sample template file and a sample json data file are given in
the `data` folder.

```bash
mustachio -t data/test.mustache -d data/test.json -o test.md
```

will output the result in `test.md`.
