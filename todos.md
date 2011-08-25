# Project TODO's

  * Define interface for injecting *convert* macros
    * these also include preprocessors
    * thinking along these lines:

```scala
// Example preprocessor
preprocessors += pre("boilerplate") { contents =>
  "My name is Philip Cali."
}

// Example parser macro
handlers += macro("cool-dude") { (dis, blocks) =>
  <div class="cool-dude-reference">
    dis.toXHTML(blocks)
  </div>
}
```

## Design TODO's

Template logic's versus Output processor's

The idea being, each defined template is passed in TemplateData, which contains
the dynamic compiled content
