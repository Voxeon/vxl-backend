use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub struct Values(Rc<RefCell<ValuesRaw>>);

#[derive(Clone, Debug, PartialEq)]
struct ValuesRaw {
    values: Vec<String>,
    parent: Option<Values>,
}

impl ValuesRaw {
    pub fn new() -> Self {
        return Self {
            values: Vec::new(),
            parent: None,
        };
    }

    pub fn new_child(parent: Values) -> Self {
        return Self {
            values: Vec::new(),
            parent: Some(parent),
        };
    }

    pub fn push_value(&mut self, name: &str) -> Option<u64> {
        let v = self.count();

        if self.value_position(name).is_some() {
            return None;
        }

        self.values.push(name.to_string());

        return Some(v);
    }

    pub fn value_depth(&self, name: &str) -> Option<u64> {
        if let Some(v) = self.value_position(name) {
            return Some(v as u64);
        }

        let v = if let Some(p) = &self.parent {
            p.value_depth(name)
        } else {
            None
        };

        return if let Some(v) = v {
            Some(v + self.count())
        } else {
            v
        };
    }

    pub fn count(&self) -> u64 {
        return self.values.len() as u64;
    }

    fn value_position(&self, name: &str) -> Option<usize> {
        return self.values.iter().position(|n| n == name);
    }
}

impl Values {
    pub fn new() -> Self {
        return Values(Rc::new(RefCell::new(ValuesRaw::new())));
    }

    pub fn new_child(parent: Values) -> Self {
        return Values(Rc::new(RefCell::new(ValuesRaw::new_child(parent))));
    }

    pub fn push_value(&mut self, name: &str) -> Option<u64> {
        return self.0.borrow_mut().push_value(name);
    }

    pub fn value_depth(&self, name: &str) -> Option<u64> {
        return self.0.borrow_mut().push_value(name);
    }

    pub fn count(&self) -> u64 {
        return self.0.borrow().count();
    }

    fn value_position(&self, name: &str) -> Option<usize> {
        return self.0.borrow().value_position(name);
    }
}
