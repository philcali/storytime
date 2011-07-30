# Project TODO's

  * Generate Meta object with meta props
    * pagination
    * custom sep
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
