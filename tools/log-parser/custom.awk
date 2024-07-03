# 最初はflag = 0
BEGIN {
    flag = 0
}
# Step という文字列が含まれる行を見つけたら、flag を 1 にする
/Step/ {
    flag = 1
}
# Loop time という文字列が含まれる行を見つけたら、flag を 0 にする
/Loop time/ {
    flag = 0
}
# flag が 1 のときだけ、行を出力する
flag == 1 {
    print $1, $5
}
