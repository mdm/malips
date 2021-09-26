use std::collections::HashSet;

pub struct LinearProgram {
    num_variables: usize,
    objective: Vec<f64>,
    inequalities: Vec<Inequality>,
}

impl LinearProgram {
    fn new(num_variables: usize) -> LinearProgram {
        LinearProgram {
            num_variables,
            objective: vec![0.0; num_variables],
            inequalities: Vec::new(),
        }
    }

    fn set_objective(&mut self, coefficients: Vec<f64>) -> Result<(), CoefficientsError> {
        if coefficients.len() == self.num_variables {
            self.objective = coefficients;

            Ok(())
        } else {
            Err(CoefficientsError)
        }
    }

    fn add_inequality(
        &mut self,
        lhs_coefficients: Vec<f64>,
        rhs: f64,
    ) -> Result<(), CoefficientsError> {
        if lhs_coefficients.len() == self.num_variables {
            let inequality = Inequality {
                lhs_coefficients,
                rhs,
            };
            self.inequalities.push(inequality);

            Ok(())
        } else {
            Err(CoefficientsError)
        }
    }

    fn solve(&self) -> Option<(Vec<f64>, f64)> {
        let objective = self.objective.clone();
        let inequalities = self.inequalities.clone();

        let mut variables = vec![0.0; self.num_variables]; // initial feasible solution
        for inequality in &inequalities {
            let slack_variable = inequality.rhs
                - variables
                    .iter()
                    .take(self.num_variables)
                    .zip(inequality.lhs_coefficients.iter())
                    .map(|(x, a)| a * x)
                    .sum::<f64>();

            variables.push(slack_variable);
        }

        let non_basic_var_indices = (0..self.num_variables).collect::<HashSet<usize>>();
        let basic_var_indices = (0..inequalities.len())
            .map(|i| i + self.num_variables)
            .collect::<HashSet<usize>>();

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
struct Inequality {
    lhs_coefficients: Vec<f64>,
    rhs: f64,
}

#[cfg(test)]
mod tests {
    use crate::LinearProgram;

    #[test]
    fn simple_example() {
        let mut lp = LinearProgram::new(3);
        lp.set_objective(vec![5.0, 4.0, 3.0]).unwrap();
        lp.add_inequality(vec![2.0, 3.0, 1.0], 5.0).unwrap();
        lp.add_inequality(vec![4.0, 1.0, 2.0], 11.0).unwrap();
        lp.add_inequality(vec![3.0, 4.0, 2.0], 8.0).unwrap();

        let solution = lp.solve().unwrap();

        assert_eq!(solution.1, 13.0);
    }
}
