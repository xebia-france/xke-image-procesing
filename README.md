Functional Image processing
======

## REPL

### I don't know what the hell is SBT!! ¯¯\\_(ツ)_/¯¯ 

Just type this

```
> ./activator
```

### C'mon! I already have everything installed

```
> sbt
```

## Testing

### How do I run the test?

Once in the _REPL_

```
> test
> testOnly fr.xebia.image.*
> testOnly fr.xebia.image.core.RawImageSpec
> ~testOnly fr.xebia.image.core.RawImageSpec
```

You are good to go!!!

------

# Working with the code

## Related to image processing

```
> testOnly fr.xebia.image.core.RawImageSpec
```

### TODO `01`
Get the first element that matches following a `left-right` / `up-down strategy`.

### TODO `02`
Replace the pixels at the specified position by the a pixel value.

### TODO `03`
Get the neighbors from a specified pixel.

## Related to front propagation

```
> testOnly fr.xebia.image.core.ImageProcessingFunctorSpec
```

### TODO `04`
Add the missing parts of the front propagation algorithm.

### TODO `05`
Count connected elements from several fronts.
