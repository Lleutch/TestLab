TestLab is a simple testing library that I have built just for fun and to try out new things.
His aim is also to be quite a simple library to use with a small API that aims to be used for small projects needing testing

## Reason behind it

As you might already know, there is a lot of testing libraries out there that work really well with F# for instance, FsCheck and Expecto.
The reason for this project is purely educational so far. Trying to work through basic F# concepts by building a small library.

## Working features

* Small library of assertion `Check`
* Basic logging, generation of test tree
* `Outcome` computation expression representing the smallest unit of composable tests

It is usable but needs a lot of love.

## Planned features

- [ ] Better logging system using logary
- [ ] Provide an API on top of it to return a `TestReportTree` element that could allow customizable reports...
- [ ] Bind report to CI servers' API
- [ ] Documentation and Sample examples
- [ ] Bind with FsCheck

## Installation

`build.cmd` and then grab the generated `.dll`
