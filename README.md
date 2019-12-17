# Context
This is an implementation of an algorithm for generating SROBDDs as seen in the slides for [this course](https://www.win.tue.nl/~hzantema/arn.html).
I wanted to implement it in Haskell because it seemed to me a perfect fit for a language that is very powerful when it comes to recursive traversals of nested data structures.
During the course of the implementation I had the feeling I should look at _recursion schemes_. I consulted following resources extensively:
* [jtobin's intro, using the `recursion-schemes` package by E. Kmett](https://jtobin.io/practical-recursion-schemes)
* [sumtypeofway's series, using the `compdata` package](https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html)
* [the great jwiegley's post on his use of recursion schemes (using the `data-fix` package) in his `hnix` package](http://newartisans.com/2018/04/win-for-recursion-schemes/)

I used the advice on pattern synonyms from the last post and also the `data-fix` package, as it seemed to me the most "no-frills".

The most important lesson I ended up taking away was about what jtobin calls the 'pattern functor' of a datatype, making it non-recursive and more versatile. My prior notion of recursion schemes was that you would need TH to generate specific fold functions for every datatype, like `foldr` for `[a]`. Extracting the recursion at the type level makes this unnecessary of course, and is really more beautiful. 

The only boilerplate really is writing the pattern synonyms and the Functor instance, but the latter is thankfully taken care of by GHC. On that note, I learned about _variance_ when looking up the [`DeriveFunctor` extension](https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/derive-functor).

Some nice resources on variance I found:
* [Variance with regard to subtyping in OO languages](https://www.stephanboyer.com/post/132/what-are-covariance-and-contravariance)
* [Variance with regard to Functors](https://www.fpcomplete.com/blog/2016/11/covariance-contravariance)

These seemingly different occurences of the term 'variance' are actually easy to unite:
Think of the subtyping relation A ≤ B as equivalent to the existence of a 'conversion function' A → B, and the former concept is subsumed in the latter.

# TODO
* Test for functional correctness (prefferably using `Quickcheck`, see perhaps [this post mentioning `Arbitrary`](https://blaxill.org/posts/compdata-trees-and-catamorphisms/))
* Test that we really reduce and share correctly (perhaps write some code to generate a visual representation, or else this should also be doable with `Quickcheck`, provided we find corresponding functional properties)
