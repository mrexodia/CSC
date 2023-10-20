int sgn(int x) {
    int res = 0;
    if(x < 0) {
        res = -1;
    } else if(x > 0) {
        res = 1;
    }
    return res;
}