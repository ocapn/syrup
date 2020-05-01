#!/usr/bin/env python

import io
import struct

class Record():
    def __init__(self, label, args):
        self.label = label
        self.args = args

    def __repr__(self):
        return "<%s: %s>" % (self.label, ", ".join(
            [repr(arg) for arg in self.args]))

class Symbol():
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return "Symbol(%s)" % self.name

def record(label, *args):
    return Record(label, args)

def peek_byte(f):
    orig_pos = bio.tell()
    byte = bio.read(1)
    bio.seek(orig_pos)
    return byte

def netstring_encode(bstr, joiner=b':'):
    return str(len(bstr)).encode('latin-1') + joiner + bstr

def syrup_encode(obj):
    if isinstance(obj, bytes):
        return netstring_encode(obj)
    elif obj is True:
        return b't'
    elif obj is False:
        return b'f'
    elif isinstance(obj, int):
        return b"i" + str(obj).encode('latin-1') + b'e'
    elif isinstance(obj, list):
        encoded_items = [syrup_encode(item) for item in obj]
        return b'(' + b''.join(encoded_items) + b')'
    elif isinstance(obj, dict):
        keys_and_encoded = [
            (syrup_encode(key), key)
            for key in obj.keys()]
        sorted_keys_and_encoded = sorted(
            keys_and_encoded,
            key=lambda x: x[0])
        encoded_hash_pairs = [
            # combine the encoded key and encode the val immediately
            ek[0] + syrup_encode(obj[ek[1]])
            for ek in sorted_keys_and_encoded]
        return b'{' + b''.join(encoded_hash_pairs) + b'}'
    elif isinstance(obj, str):
        return netstring_encode(obj.encode('utf-8'),
                                joiner=b'"')
    elif isinstance(obj, Symbol):
        return netstring_encode(obj.name.encode('utf-8'),
                                joiner=b"'")
    # Only double is supported in Python.  Single-precision not supported.
    elif isinstance(obj, float):
        return b'D' + struct.pack('>d', obj)
    elif isinstance(obj, Record):
        return b'<' + \
            syrup_encode(obj.label) +\
            b''.join([syrup_encode(x) for x in obj.args]) + \
            b'>'
    elif isinstance(obj, set):
        encoded_items = [syrup_encode(x) for x in obj]
        return b'#' + b''.join(sorted(encoded_items)) + b'$'
    else:
        raise ValueError("Unsupported type: %r" % obj)


zoo_structure = record(
    b"zoo",
    "The Grand Menagerie",
    [{Symbol("species"): b"cat",
      Symbol("name"): "Tabatha",
      Symbol("age"): 12,
      Symbol("weight"): 8.2,
      Symbol("alive?"): True,
      Symbol("eats"): {b"mice", b"fish", b"kibble"}},
     {Symbol("species"): b"monkey",
      Symbol("name"): "George",
      Symbol("age"): 6,
      Symbol("weight"): 17.24,
      Symbol("alive?"): False,
      Symbol("eats"): {b"bananas", b"insects"}}])
    
