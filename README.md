# Introduction

This repository contains a lint tool for the hardware description language alogic.

# Usage

sbt is a build tool for Scala.

sbt "run <path_to_header_file>* <path_to_file_to_compile>"

# DEPRECATED

I found the parser combinator library a bit slow (12 seconds to compile).

The new code is at: https://github.com/peterderivaz/alogic_antlr
