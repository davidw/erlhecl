set foo 1

proc bee {a b c} {
    puts "$a $b $c"
}

set bar [+ $foo 1]

puts [format "A B C ~B" [int $foo]]
puts [format "A B C ~B" [int "2"]]

# set foo

# bee

puts foo${foo}

puts [+ 2 [+ 2 1]]

puts {wahoo
    blah blah
}

puts $foo${bar}
# This is a comment

if { == $foo 1 } {
    puts "bar"
}

if { == $foo 2 } {
    puts "true"
} else {
    puts "false"
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