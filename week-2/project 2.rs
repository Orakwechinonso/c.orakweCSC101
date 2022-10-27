fn main() {
  // t represents toshiba , m represents mac , h represents hp , d represents dell , a represents acer
    let n1 = 2;
    let n2 = 1;
    let n3 = 3;
    let n4 = 3;
    let n5 = 1;
    
    let a1 = 450_000;
    let a2 = 1_500_000;
    let a3 = 750_000;
    let a4 = 2_850_000;
    let a5 = 250_000;
    
    let t = n1 * a1; 
    let m = n2 * a2;
    let h = n3 * a3;
    let d = n4 * a4;
    let a = n5 * a5;
    
//s is the sum of sales record, b is the average of sales record
    let s = t + m + h + d + a;
    println!("sum of sales record is {}", s);
    let b = s/5;
    println!("average of sales record is {}",s);
}