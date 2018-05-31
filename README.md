# better-monadic-files

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)


Typesafe File IO with monad of your choice


## Features

 * Separate types for plain file and directory
 * Only methods with side effect return a monad
 * Uses `cat-core`'s `MonadError[F[_], Throwable]` for flexibility
 * Backed by [better-files](https://github.com/pathikrit/better-files) with conversions from and to 
 the backing `better.files.File`

NOTE: This is not a drop-in replacement for `better-files`! Use this only if you prefer 
typelevel style FP.

## Getting started

Doing IO is hard so why not let the compiler guide us?  

Lift any `better.files.File` instance to a `FileM[_]` by:

```scala
import better.files.File
import net.kurobako.bmf.PathM._
import cats.effect.IO

val file = File.home / "foo"
val x: IO[(FileM[IO], Long)] = 
    for {
        foo <- file.liftFile[IO].create()
        _ <- foo.append("foo")
        length <- foo.size
    } yield (foo, length)
```



## How to build

Prerequisites:

 * JDK 8
 * sbt 1.x

The project uses sbt for build so you will need to install the latest 1.x branch of sbt.

Clone the project and then in project root

    sbt assemble
    
## Motivation

Got tired of IO related call throwing exceptions. 

## Licence

    Copyright 2018 WEI CHEN LIN
    
    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at
    
       http://www.apache.org/licenses/LICENSE-2.0
    
    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.