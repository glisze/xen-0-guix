(
D="grub-mkimage --format=x86_64-xen ... --output=grub-x86_64-xen.bin ...";
a="$( dirname $( dirname $( readlink -e $( which grub-mkimage ))))";
b="grub-x86_64-xen.bin";
c="/boot/grub";
d="./grub.cfg";
    grub-mkimage --config=$d --format=x86_64-xen --directory="$a/lib/grub/x86_64-xen/" --output="./$b" --prefix="$c" $a/lib/grub/x86_64-xen/*.mod
)
