# PetitScript

[![Test CI](https://github.com/github/docs/actions/workflows/test.yml/badge.svg)](https://github.com/LucasPickering/petitscript/actions)
[![crates.io](https://img.shields.io/crates/v/petitscript.svg)](https://crates.io/crates/petitscript)
[![docs.rs](https://img.shields.io/docsrs/petitscript)](https://docs.rs/petitscript)

A minimal and embeddable _subset_ of JavaScript. PetitScript is in very early development, and is not yet ready for any sort of use.

## What is PetitScript?

PetitScript is a minimal (let me say that again: minimal!) subset of JavaScript targeting simple applications such as dynamic configuration and embedded scripts. PS aims to fill a similar niche as [KDL](https://kdl.dev/), [KCL](https://github.com/kcl-lang/kcl), [Nickel](https://nickel-lang.org/), [HCL](https://github.com/hashicorp/hcl), and others with one distinct advantage: you already know it. No new syntax, no new semantics. It's JavaScript, but without the thorns. Or the flowers. Or the leaves. By the branches are still there, and by golly are they familiar. Some key features of PetitScript are:

- Strict subset of JavaScript. If it runs in PS, it runs on Node. See the [Compatibility Guarantee](#compatibility-guarantee)
- Purely immutable semantics
- Functional paradigms. It's hard to be imperative without mutability!
- Full compatibility with JavaScript tooling. Keep using Prettier!

PetitScript intentionally uses extremely simple semantics in order to be approachable and maintainable. It's designed for applications that you only touch once a week. No one wants to remember wonky syntax for a language they aren't spending their whole day in. Better yet, no one wants to _learn_ wonky syntax for a language they only need once a _year_.

To that end, PetitScript **lacks** these features:

- Mutable values
- Mutable bindings
- Classes
- Async/await
- Generators
- Third-party dependencies. PetitScript supports local imports to enable code reuse, but you never have to worry about package management.
- And many more!!!

### What is PetitScript _not_?

JavaScript is a labrador. It'll do anything for you. Sometimes it shits on the rug, but you've just gotta shrug. No one's perfect.

C is a pitbull. If you can control it, it's unstoppable and your neighbors will be terrified of you. But if you mess up, it'll bite your hand off.

PetitScript is a turtle. You feed it once a week and then forget about it. It _wants_ to be forgotten. It does one thing and it does it well. If you ask it to do something else, it won't. It's a turtle. Why are you talking to it?

Seriously, PetitScript is not a general purpose programming language. Don't try to build your startup on it.

## Compatibility Guarantee

_Or your money back!!_

## A note on the name

This is a programming language named PetitScript. The name of this language is PetitScript. This name can be abbreviated as PS. The name of this language is _not_ Petitscript. Nor is it PetitJS.

I chose the name PetitScript in order to imply an association with JavaScript while indicating a level of distinction commensurate with the minimalness of the language. The name "PetitJS" may imply "It's like JS, but smaller!". While true, this statement may undersell just how much _smaller_ it is.

Despite the name, PetitScript files still use the `.js` extension. This is because of the stated goal of making it as easy as possible to pick up and write PS; by reusing the existing `.js` extension, you get whatever syntax highlighting/completion/etc. your editor already provides for JS without any additional configuration.

If it's confusing, I'm sorry. I tried.
