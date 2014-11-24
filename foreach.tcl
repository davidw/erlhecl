set lol2 [list a b c [list x y z]]

puts "List is: $lol2"

foreach X $lol2 {
    puts $X
}

foreach E $lol2 {
    foreach F $E {
	puts "<tr> <td> $E </td <td> $F </td></tr>"
    }
}

#set lol1 {1 2 3 {4 5 6}}
#puts $lol1
#foreach L $lol1 {
#    puts $L
#}