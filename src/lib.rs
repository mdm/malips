pub struct LinearProgram {
    num_variables: usize,
    objective: Vec<f64>,
    constraints: Vec<Constraint>,
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
            let mut constraint = Constraint {
                index: 1 + self.num_variables + self.constraints.len(),
                coefficients: vec![rhs],
            };
            constraint
                .coefficients
                .extend(lhs_coefficients.iter().map(|coefficient| -coefficient));

            self.objective.push(0.0);
            self.constraints.push(constraint);

            for i in 0..self.constraints.len() {
                while self.constraints[i].coefficients.len()
                    < 1 + self.num_variables + self.constraints.len()
                {
                    self.constraints[i].coefficients.push(0.0);
                }
            }

            Ok(())
        } else {
            Err(CoefficientsError)
        }
    }

    pub fn solve(mut self) -> Option<(Vec<f64>, f64)> {
        dbg!(&self.objective, &self.constraints);

        // TODO: detect case where we have no constraints => unbounded program

        if !self.solve_phase1() {
            return None; // program is infeasible
        }

        dbg!(&self.objective, &self.constraints);

        self.pivot_until_solved();

        let mut solution = Vec::new();
        for i in 1..(1 + self.num_variables) {
            if let Some(constraint) = self
                .constraints
                .iter()
                .find(|constraint| constraint.index == i)
            {
                solution.push(constraint.coefficients[0]);
            } else {
                solution.push(0.0);
            }
        }

        Some((solution, self.objective[0]))
    }

    fn solve_phase1(&mut self) -> bool {
        if self
            .constraints
            .iter()
            .all(|constraint| constraint.coefficients[0] >= 0.0)
        {
            return true;
        }

        // set up and solve auxiliary program

        let mut auxiliary_objective = vec![0.0; self.objective.len()];
        auxiliary_objective.push(-1.0);

        let original_objective = std::mem::replace(&mut self.objective, auxiliary_objective);

        for i in 0..self.constraints.len() {
            self.constraints[i].coefficients.push(-1.0);
        }

        let initial_entering_variable_index = self.objective.len();
        let initial_leaving_variable_index = self
            .constraints
            .iter()
            .enumerate()
            .min_by(|(_, constraint_a), (_, constraint_b)| {
                constraint_a.coefficients[0].partial_cmp(&constraint_b.coefficients[0]).expect("Expected basic variable not to be NaN.")
            })
            .expect("Expected to find a most infeasible constraint.")
            .0;

        self.pivot(initial_entering_variable_index, initial_leaving_variable_index);
        self.pivot_until_solved();

        // transform back into feasible original program

        for i in 0..self.constraints.len() {
            self.constraints[i].coefficients.pop();
        }

        // TODO: fix original_objective

        unimplemented!()
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
            if self.constraints[i].coefficients[entering_variable_index] >= 0.0 {
                continue;
            }

            let upper_bound = self.constraints[i].coefficients[0]
                / -self.constraints[i].coefficients[entering_variable_index]; // TODO: handle division by 0
            dbg!(upper_bound);
            if leaving_variable_index.is_none() || upper_bound < least_upper_bound {
                leaving_variable_index = Some(i);
                least_upper_bound = upper_bound;
            }
        }

        assert!(leaving_variable_index.is_some());
        dbg!(least_upper_bound);
        leaving_variable_index.unwrap()
    }

    fn pivot(&mut self, entering_variable_index: usize, leaving_variable_index: usize) {
        let index = self.constraints[leaving_variable_index].index;
        self.constraints[leaving_variable_index].coefficients[index] = -1.0;

        let update_coefficients =
            |old_coefficients: &Vec<f64>, leaving_variable_coefficients: &Vec<f64>| {
                let scaling_factor = old_coefficients[entering_variable_index]
                    / -leaving_variable_coefficients[entering_variable_index];
                old_coefficients
                    .iter()
                    .zip(leaving_variable_coefficients.iter())
                    .map(|(old_coefficient, leaving_variable_coefficient)| {
                        old_coefficient + scaling_factor * leaving_variable_coefficient
                    })
                    .collect()
            };

        self.objective = update_coefficients(
            &self.objective,
            &self.constraints[leaving_variable_index].coefficients,
        );
        for i in 0..self.constraints.len() {
            if i != leaving_variable_index {
                self.constraints[i].coefficients = update_coefficients(
                    &self.constraints[i].coefficients,
                    &self.constraints[leaving_variable_index].coefficients,
                );
            }
        }

        // rearrange coefficients for entering variable constraint
        self.constraints[leaving_variable_index].index = entering_variable_index;
        let scaling_factor =
            1.0 / -self.constraints[leaving_variable_index].coefficients[entering_variable_index];
        self.constraints[leaving_variable_index].coefficients[entering_variable_index] = 0.0;
        for i in 0..self.constraints[leaving_variable_index].coefficients.len() {
            self.constraints[leaving_variable_index].coefficients[i] *= scaling_factor;
        }
    }

    fn pivot_until_solved(&mut self) {
        while let Some(entering_variable_index) = self.select_entering_variable() {
            dbg!(entering_variable_index);

            let leaving_variable_index = self.select_leaving_variable(entering_variable_index);
            dbg!(leaving_variable_index);

            self.pivot(entering_variable_index, leaving_variable_index);
            dbg!(&self.objective, &self.constraints);
            // break;
        }
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

#[derive(Debug, Clone)]
struct Constraint {
    index: usize,
    coefficients: Vec<f64>,
}

#[cfg(test)]
mod tests {
    use crate::LinearProgram;

    #[test]
    fn all_rhs_non_negative_1() {
        let mut lp = LinearProgram::new(vec![5.0, 4.0, 3.0]);
        lp.add_constraint(vec![2.0, 3.0, 1.0], 5.0).unwrap();
        lp.add_constraint(vec![4.0, 1.0, 2.0], 11.0).unwrap();
        lp.add_constraint(vec![3.0, 4.0, 2.0], 8.0).unwrap();

        let solution = lp.solve().unwrap();

        assert_eq!(solution.0, vec![2.0, 0.0, 1.0]);
        assert_eq!(solution.1, 13.0);
    }

    #[test]
    fn all_rhs_non_negative_2() {
        let mut lp = LinearProgram::new(vec![6.0, 8.0, 5.0, 9.0]);
        lp.add_constraint(vec![2.0, 1.0, 1.0, 3.0], 5.0).unwrap();
        lp.add_constraint(vec![1.0, 3.0, 1.0, 2.0], 3.0).unwrap();

        let solution = lp.solve().unwrap();

        assert_eq!(solution.0, vec![2.0, 0.0, 1.0, 0.0]);
        assert_eq!(solution.1, 17.0);
    }
}
