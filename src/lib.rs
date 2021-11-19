use std::collections::HashSet;

pub struct LinearProgram {
    num_variables: usize,
    objective: Vec<f64>,
    constraints: Vec<Vec<f64>>,
}

impl LinearProgram {
    pub fn new(objective_coefficients: Vec<f64>) -> LinearProgram {
        let num_variables = objective_coefficients.len();

        let mut objective = vec![0.0];
        objective.extend(objective_coefficients);

        LinearProgram {
            num_variables,
            objective,
            constraints: Vec::new(),
        }
    }

    pub fn add_constraint(
        &mut self,
        lhs_coefficients: Vec<f64>,
        rhs: f64,
    ) -> Result<(), CoefficientsError> {
        if lhs_coefficients.len() == self.num_variables {
            let mut constraint = vec![rhs];
            constraint.extend(lhs_coefficients.iter().map(|coefficient| -coefficient));

            self.objective.push(0.0);
            self.constraints.push(constraint);

            for i in 0..self.constraints.len() {
                while self.constraints[i].len() < 1 + self.num_variables + self.constraints.len() {
                    self.constraints[i].push(0.0);
                }
            }

            Ok(())
        } else {
            Err(CoefficientsError)
        }
    }

    pub fn solve(&self) -> Option<(Vec<f64>, f64)> {
        let mut variables = self.initialize_variables();

        dbg!(&self.objective, &self.constraints, &variables);

        while let Some(entering_variable_index) = self.select_entering_variable() {
            let leaving_variable_index = self.select_leaving_variable(entering_variable_index);

            dbg!(entering_variable_index, leaving_variable_index);
            break;
        }

        None
    }

    fn initialize_variables(&self) -> Vec<f64> {
        let mut variables = vec![1.0];

        for _ in 0..self.num_variables {
            variables.push(0.0);
        }

        for _ in 0..self.constraints.len() {
            variables.push(0.0); // temporary value, reassigned in next loop
        }

        for i in 0..self.constraints.len() {
            variables[1 + self.num_variables + i] = self.constraints[i]
                .iter()
                .zip(variables.iter())
                .map(|(coefficient, variable)| coefficient * variable)
                .sum(); // TODO: do we need to sum all of these or is the first term sufficent?
        }

        variables
    }

    fn select_entering_variable(&self) -> Option<usize> {
        for i in 1..self.objective.len() {
            if self.objective[i] > 0.0 {
                return Some(i);
            }
        }

        None
    }

    fn select_leaving_variable(&self, entering_variable_index: usize) -> usize {
        let mut leaving_variable_index = None;
        let mut least_upper_bound = std::f64::INFINITY;
        
        for i in 0..self.constraints.len() {
            if self.constraints[i][entering_variable_index] > 0.0 {
                continue;
            }

            assert!(self.constraints[i][entering_variable_index] < 0.0);
            
            let upper_bound = self.constraints[i][0] / -self.constraints[i][entering_variable_index]; // TODO: handle division by 0
            if leaving_variable_index.is_none() || upper_bound < least_upper_bound {
                leaving_variable_index = Some(i);
                least_upper_bound = upper_bound;
            }
        }

        assert!(leaving_variable_index.is_some());
        leaving_variable_index.unwrap()
    }

    fn pivot(&mut self, entering_variable_index: usize, leaving_variable_index: usize) {
        unimplemented!();
    }
}

#[derive(Debug)]
pub struct CoefficientsError;

impl std::fmt::Display for CoefficientsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        "number of coefficients does not match number of variables".fmt(f)
    }
}

impl std::error::Error for CoefficientsError {
    fn description(&self) -> &str {
        "number of coefficients does not match number of variables"
    }
}

#[derive(Clone)]
struct Constraint {
    lhs_coefficients: Vec<f64>,
    rhs: f64,
}

#[cfg(test)]
mod tests {
    use crate::LinearProgram;

    #[test]
    fn simple_example() {
        let mut lp = LinearProgram::new(vec![5.0, 4.0, 3.0]);
        lp.add_constraint(vec![2.0, 3.0, 1.0], 5.0).unwrap();
        lp.add_constraint(vec![4.0, 1.0, 2.0], 11.0).unwrap();
        lp.add_constraint(vec![3.0, 4.0, 2.0], 8.0).unwrap();

        let solution = lp.solve().unwrap();

        assert_eq!(solution.1, 13.0);
    }
}
