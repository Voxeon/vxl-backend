pub trait Builder {
    type ErrorType;
    type Output: Into<Vec<u8>>;

    
}
