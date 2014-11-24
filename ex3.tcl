set bee { foo [ bar }
puts $bee

set lol1 {a b c {x y z}}
puts $lol1
#foreach A $lol1 {
 #   puts "LOL $A"
#}

set lol2 [list a b c [list x y z]]
puts ">> $lol2"
puts [format "~p" $lol2]
foreach B $lol2 {
    foreach X $B {
	puts "LOL2 $B $X"
    }
}

