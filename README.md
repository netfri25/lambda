# Lambda Calculus Evaluator
## About
[Lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) is a formal system for expressing computation based on function abstraction and application using variable binding and substitution (Wikipedia).
This project is a personal project that I do for my own fun.
I suggest to not use it for anything other than fun.

### Inspiration
I was inspired by [this video](https://www.youtube.com/watch?v=RcVA8Nj6HEo) to start writing a lambda calculus evaluator, and I plan on creating a visualizer similar to the visualization shown in that video. I wanted to create a lambda calculus evaluator for a long time already, but only now I have the time for it.

## Known Bug
currently, the beta reduction doesn't work well in when there's substitution of a variable and there's already a variable with the same name in that scope.
This will be fixed in the next few commits.

## TODO
 - [x] add a REPL
 - [ ] use a [`SlotMap`](https://docs.rs/slotmap/latest/slotmap/). This approach fixes the known bug mentioned above, while also allowing shared mutability without the use of `Rc<RefCell<T>>`.
 - [ ] implement the visualization, as shown in [this video](https://www.youtube.com/watch?v=RcVA8Nj6HEo)
