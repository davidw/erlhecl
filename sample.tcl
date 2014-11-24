puts [float "4.56"]

set foo 1

puts $foo

set bar [+ $foo 1]

puts $bar

puts "Foo is $foo and bar is $bar"

set l [list a b c d]

puts $l

set lol [list a b c [list x y z]]

puts $lol

puts [list 1 2 3 [list 4 5 6]]

puts [format "A B C ~B" [int $foo]]
puts [format "A B C ~B" [int "2"]]

# This is a comment!

puts foo${foo}

puts [+ 4 [+ 2 1]]

puts {A
multi-line
block
}

if { == $foo 1 } {
    puts "'== $foo 1' evaluates to true"
}

set cmd {== $foo 2}

if $cmd {
    puts "$cmd evaluates to true"
} else {
    puts "$cmd evaluates to false"
}

set foo 0
puts "Foo is $foo"
while { < $foo 5 } {
    puts "Foo is $foo "
    set foo [+ $foo 1]
}

foreach x [list 1 2 3] {
    foreach y [list a b c] {
	puts "${x} $y "
    }
}

set bar 0
while { true } {
    puts "Bar is $bar"
    set bar [+ $bar 1]
    if { == $bar 10 } {
	break
    }
}

#proc recursive {arg} {
#    if { == $arg 1 } {
#	puts $arg
#    } else {
#	recursive [- $arg 1]
#	puts $arg
#    }
#}

#recursive 5

proc dostuff {a b} {
    puts "A is $a and B is $b"
}

dostuff 5 "hello world"

puts {a b {d e} {f g}}

foreach a {{1 2} {2 3} {3 4}} {
    foreach b $a {
	puts "B is $b"
    }
    puts $a
}


foreach a [list [list 1 2] [list 2 3] [list 3 4]] {
    foreach b $a {
	puts "B is $b"
    }
    puts $a
}

foreach a {1 2 3 4} {
    puts "<tr><td>ROW $a </td></tr>"
}

set i 2
set l {1 2 3 4}
puts "Index $i of $l is [index $l $i]"

puts " \"quoted text\" "

puts [float "4.56"]

puts [sort {q w e r t y u i o p aa}]

rename puts foo

foo bar