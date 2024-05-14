# Draft Syrup Specification

## Status of This Document

This is a draft specification, created by the [OCapN](https://github.com/ocapn/ocapn) pre-standardization group. This draft is subject to change with the work of the group; if you're interested in being part of that work, please join!

## Overview

Syrup is a lightweight and easy-to-implement data serialization format. A serialized Syrup structure is canonicalized; the same data thus always serializes to the same set of octets (8 bit values).

Syrup is a *binary* serialization format; data is not encoded within text, but as the raw underlying data, preventing common escaping bugs. Syrup is an easy way to canonicalize; unordered data types are sorted by the octet, which is simple to implement.

Syrup is a serialization of the [Preserves](https://preserves.gitlab.io/preserves/) data model. However, knowledge of Preserves is not required to use Syrup.

### Syrup at a glance

The comprehensive specification is below, however here's a birds-eye view of all the types supported in Syrup:

```text
Booleans:           t or f
32-bit floats:      F<ieee-binary32>              (big endian)
64-bit floats:      D<ieee-binary64>              (big endian)
Positive integers:  <decimal-digits>+
Negative integers:  <decimal-digits>-
Binary data:        3:cat
Strings:            6"björn                       (utf-8 encoded)
Symbols:            6'update                      (utf-8 encoded)
Dictionaries:       {<key1><val1><key2><val2>}    (sorted by serialized key)
Sequences:          [<item1><item2><item3>]
Records:            <<label><val1><val2><val3>>   (the outer <> actually appear)
Sets:               #<item1><item2><item3>$       (sorted by serializations)
```

`<` and `>` with only letter/digit/hyphen characters in between them represent a serialized value, but appear literally as the first and last octets (respectively) of a record serialization.

## Specification

Syrup uses several ASCII characters to indicate types and encode data length. Such characters are represented in this document as `monospace spans` and appear in serializations as the corresponding individual octets (8 bit values).

### Booleans

Booleans are serialized as `t` for true, or `f` for false.

### 32-bit Floats

All [IEEE 754](https://ieeexplore.ieee.org/document/4610935) binary32 ("single precision") floating point values are serialized with an `F` followed by 4 octets representing the value in big endian order.

### 64-bit Floats

All [IEEE 754](https://ieeexplore.ieee.org/document/4610935) binary64 ("double precision") floating point values are serialized with a `D` followed by 8 octets representing the value in big endian order.

### Positive Integers

Integers are serialized as a base 10 string format beginning with the most significant digit until the least significant digit. The integer is then followed by a `+` character. Positive integers have no upper bound.

The integer 0 (zero) is considered positive.

#### Example

The number 0 is serialized as `0+` and the number 72 is serialized as `72+`.

### Negative Integers

Negative integers are serialized as a base 10 string format beginning with the most significant digit until the least significant digit. The integer is then followed by a `-` character. Negative integers have no lower bound.

Note that -0 (negative zero) is not a valid negative integer.

#### Example

The number -5 (negative five) is serialized as `5-`.

### Binary Data

Binary data is a sequence of octets. These octets can represent any kind of data, such as images, sound, etc.

Binary data is serialized as the size of the data (number of octets), followed by a `:` and then the constituent octets.

The size is a sequence of ASCII decimal digits beginning with the most significant digit until the least significant digit.

#### Examples

Due to the nature of binary data being a arbitrary sequence of octets and not encoding text, it is difficult to show examples within the specification (a text document). However, since examples are important, we've tried to demonstrate as best as we can:

- an ASCII string with the content `cat` would be serialized as `3:cat` (note: strings are better formatted with the String data type).
- a 10 KB PDF would be serialized as `10000:%PDF-<remaining-data>` (PDF data starts with 0x25 0x50 0x44 0x46 0x2D).
- a 32 MiB JPEG would be `33554432:<jpeg-data>`

### Strings

Strings are textual information consisting of [Unicode scalar values](https://unicode.org/glossary/#unicode_scalar_value).

Strings are serialized as the size of the data (number of octets), followed by a `"` and then the constituent UTF-8 octets.

The size is a sequence of ASCII decimal digits beginning with the most significant digit until the least significant digit.

#### Examples

Here are some examples of strings and how they'd be serialized:

- "bear" would be as `4"bear`
- "björn" would be as `6"björn` (`ö` is represented in UTF-8 as U+00F6 which is two octets).
- "熊" would be `3"熊` (`熊` is represented in UTF-8 as U+718A which is three octets).

### Symbols

Symbols are string-like values which represent identifiers.

Symbols are serialized as the size of the data (number of octets), followed by a `'` and then the UTF-8 octets representing the symbol's text.

The size is a sequence of ASCII decimal digits beginning with the most significant digit until the least significant digit.

#### Examples

- A symbol with the text `fetch` would be encoded as `5'fetch`
- A symbol with the text `hämta` would be encoded as `6'hämta` (`ä` is represented in UTF-8 as U+00E4 which is two octets).

### Dictionaries

Dictionaries are unordered maps between keys and values representing structures where keys can be easily looked up to retrieve values. Any valid syrup value can be used as a key or value in a dictionary, and a dictionary may be empty (i.e., consist of zero keys and zero values).

The serialization of a dictionary begins with a `{`, followed by the serialization of each successive key-value pair as ordered by sorting, and finally ends with a `}`. The serialization of a key-value pair is the serialization of the key according to its type and then the serialization of the value according to its type, with nothing in between the two.

In order to ensure the same dictionary always serializes to the same sequence of octets (its canonicalized form), dictionary members are sorted by the serialization of their keys. Refer to the [Sorting Algorithm section](#sorting-algorithm) for details.

#### Example

The following JSON:

```json
{
    "name": "Alice",
    "age": 30,
    "isAlive": true
}
```

would serialize to:

```syrup
{3:age30+4:name5:Alice7:isAlivet}
```

Note that the keys occur in the following order: `age`, `name`, and `isAlive` due to sorting.

### Sequences

The serialization of a sequence begins with a `[`, followed by the serialization of each successive item, and finally ends with a `]`.

#### Example

The following JSON:

```json
[1,2,3]
```

Would be serialized in syrup as:

```syrup
[1+2+3+]
```

### Records

A record begins with a `<`, then followed by the record label which is serialized according to its type, followed by each value in the fields one after the other according to the serialization of the respective type, and finally a `>` character.

#### Example

A record with the label being the symbol `person` followed by three fields with the values `Alice` (string), `30` (positive integer), and `true` (boolean)
would be serialized as:

```syrup
<6:person5:Alice30+t>
```

### Sets

Sets are unordered collections of unique values that can be easily tested to see if they include any particular value.

The serialization of a set begins with a `#`, followed by the serialization of each successive item as ordered by sorting, and finally ends with a `$`.

Like dictionaries, the items of sets must be sorted by their serialization to ensure canonical results. Refer to the [Sorting Algorithm section](#sorting-algorithm) for details.

#### Example

The set with the members `3`, `2`, and `1` (all positive integers) would be serialized as:

```syrup
#1+2+3+$
```

## [Sorting Algorithm](#sorting-algorithm)

Sorting is used by certain Syrup data types to ensure values are in their canonicalized form. Sorting is done by first each value is serialized to their respective Syrup binary representations and then sorted from lowest to highest in value.

Below is an algorithm that takes two sequences of octets and says if the first sequence of octets is smaller than the second sequence.

### Less than algorithm

The algorithm to calculate if `s1` (sequence 1) is less than `s2` (sequence 2):

1.  Calculate the number of octets in `s1` and define that as `s1_length`
2.  Calculate the number of octets in `s2` and define that as `s2_length`
3.  Define an `index` with a value of `0`
4.  Return `false` if (`s1_length` is the same as `index`) and (`s2_length` is the same as `index`).
5.  return `true` if `s1_length` is the same as `index`
6.  Return `false` if `s2_length` is the same as `index`
7.  Define `octet1` with the value of the octet at the index `index` in `s1`
8.  Define `octet2` with the value of the octet at the index `index` in `s2`
9.  Return `true` if `octet1` is numerically lower in value than `octet2`
10. Return `false` if `octet1` is numerically greater in value than `octet2`
11. Increment `index` by 1 and jump to step 4.

The same algorithm is also written below in pseudocode.

#### Pseudocode

``` text
function is_less_than(s1, s2) {
    s1_length = amount_of_octets(s1)
    s2_length = amount_of_octets(s2)

    index = 0

    loop {
        // If we've reached the end of both byte strings, s1 is not less than s2.
        if (index == s1_length AND index == s2_length) {
            return false
        }

        // If we've reached the end of s1, but not s2 then s1 is smaller.
        if (index >= s1_length) {
            return true
        }

        // If we've reached the end of s2, but not s1, then s1 is bigger.
        if (index >= s2_length) {
            return false
        }

        // We are not at the end of either sequence so compare the next octets.
        // Extract the octet at the position index from each sequence
        octet1 = octet_at(sequence: s1, position: index)
        octet2 = octet_at(sequence: s2, position: index)

        if (octet1 < octet2) {
            return true
        }
        if (octet1 > octet2) {
            return false
        }

        index = index + 1
    }
}
```
