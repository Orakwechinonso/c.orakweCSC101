<<<<<<< HEAD
fn main() {
    let p:f64 = 520_000_000.0;
    let r:f64 = 10.0;
    let t:f64 = 5.0;

    let apr:f64 = 1.0 + (r / 100.0);
    let apr = apr*apr*apr*apr*apr;
    let a = p * apr;
    println!("amount is {}",a);
    let ci = a - p;
    println!("compond interest is {}", ci);
=======
fn main() {
    let p:f64 = 520_000_000.0;
    let r:f64 = 10.0;
    let t:f64 = 5.0;

    let apr:f64 = 1.0 + (r / 100.0);
    let apr = apr*apr*apr*apr*apr;
    let a = p * apr;
    println!("amount is {}",a);
    let ci = a - p;
    println!("compond interest is {}", ci);
>>>>>>> 3572cb2896bae23abb17e2362b440c23e1bc0d5a
}