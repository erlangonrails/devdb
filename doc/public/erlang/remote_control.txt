几种接入erlang shell的方式:

<1> JCL (Job Control Language)
启动一个erlang节点, 在后台运行 
erl -sname n1 -detached -setcookie abc
启动第二个erlang节点
erl -sname n2 -setcookie abc
<Ctrl + G>进入JCL
-->h  显示帮助
-->r 'n1@liqiang'
-->c 连接到n1节点

<2> remsh方式:
erl -sname n2 -setcookie abc -remsh n1@liqiang (使用Ctrl + C退出attached模式)
