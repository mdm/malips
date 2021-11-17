use std::collections::HashSet;

pub struct LinearProgram {
    num_variables: usize,
    objective: Vec<f64>,
    constraints: Vec<Vec<f64>>,
}

impl LinearProgram {
    fn new(objective_coefficients: Vec<f64>) -> LinearProgram {
        let num_variables = objective_coefficients.len();

        let mut objective = vec![0.0];
        objective.extend(objective_coefficients);

        LinearProgram {
            num_variables,
            objective,
            constraints: Vec::new(),
        }
    }

    fn add_constraint(
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
                self.constraints[i].push(0.0);
            }

            Ok(())
        } else {
            Err(CoefficientsError)
        }
    }

    fn solve(&self) -> Option<(Vec<f64>, f64)> {
        let objective = self.objective.clone();
        let constraints = self.constraints.clone();

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
                .sum();
        }

        dbg!(&variables);

        None
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
