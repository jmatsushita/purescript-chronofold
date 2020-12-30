# Work In Progress. purescript-chronofold

<!-- [![Latest release](http://img.shields.io/github/release/purescript/purescript-enums.svg)](https://github.com/purescript/purescript-enums/releases)
[![Build status](https://github.com/purescript/purescript-enums/workflows/CI/badge.svg?branch=master)](https://github.com/purescript/purescript-enums/actions?query=workflow%3ACI+branch%3Amaster)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-enums/badge)](https://pursuit.purescript.org/packages/purescript-enums) -->

Chronofold data structure in Purescript.

Work is currently done in the open on Twitch: https://twitch.tv/junxan and archived on Youtube: https://www.youtube.com/channel/UCE5-DI3ORnWqPyiiKFLrTGg 

> find out about other declarative programming streamers https://fpers.vercel.app/

## Installation

TBD

## Usage

TBD

## Open Questions

Check with author and rust implementation:
 - unclear which operations are on on write and/or on read:
   - linked list relinking
   - preemptive CT sibling insertion
 - the timestamp of a new locally authored op is greater than  other timestamps in the log. That excludes the case of preemptive  siblings, so ndx−1α is not needed. The index of the  preceding character should be already known, so ndxα is not needed either.
   - Don't we need ndxInv to check if the op is an insertion or an append? https://github.com/jmatsushita/purescript-chronofold/blob/277f005b0518ee06a0af371ee21139123c8b9394/src/Data/Chronofold/Core.purs#L184

## Documentation

<!-- Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-chronofold). -->