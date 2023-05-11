# `reuron-io`

The server backing API endpoints used by reuron, and a static
build of reuron as a web app.

It is forked from the [`grace`](https://github.com/gabriella437/grace) project.

This server is deployed to https://grace.io, which is hosted on fly.io.

## Endpoints

### `/interpret`

`echo $SOME_EXPRESSION | https://reuron.io/interpret` --data-binary @-`

Takes a `reuron` expression and normalizes it into JSON. A `reuron` expression representing a scene can be rendered in the Reuron app.

### `convert-swc`


``` bash
echo $SOME_PATH_OR_URL | \
  https://reuron.io/convert-swc?degree=10` --data-binary @-`
```

Read the contents of an .swc file and generate a `reuron` expression
representing a neuron.

The `degree` parameter determines how much simplification to perform
on the neuron. `degree=10` means that only 1 in 10 "boring" segments
will be retained. A "boring" segment is not the root, not a leaf,
and not a branch point. The `parent` property of each retained segment
is updated to the nearest retained ancestor.
