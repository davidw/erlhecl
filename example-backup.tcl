set foo 1

proc bee {a b c} {
    puts "A is $a B is $b $c"
}

bee 1 2 3

set bar [+ $foo 1]

puts [format "A B C ~B" [int $foo]]
puts [format "A B C ~B" "2"]

# set foo

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

foreach x {1 2 3} {
    foreach y {a b c} {
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

#proc puts {stuff} {

puts Foo
rename puts bar
bar Bar

proc puts {text} {
    bar $text
}

puts Junk

puts [list a b c d]

foreach A [list 1 2 3 4] {
    puts "A is now $A"
}

set memory {a b c d}

foreach B $memory {
    puts "Memory value is $B"
}

set lol1 {a b c {x y z}}
foreach A $lol1 {
    puts "LOL $A"
}

set lol2 [list a b c [list x y z]]
puts [format "~p" $lol2]
foreach B $lol2 {
    puts "LOL2 $B"
}

puts $lol2