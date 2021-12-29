#[cfg(test)]
#[macro_use]
extern crate approx;

#[derive(Debug)]
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
        println!("Before solving:\n{}", self);

        // TODO: detect case where we have no constraints => unbounded program

        if !self.solve_phase1() {
            return None; // program is infeasible
        }

        println!("After phase 1:\n{}", self);

        if self.pivot_until_solved() {
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
        } else {
            Some((vec![std::f64::INFINITY; self.num_variables], std::f64::INFINITY)) // program is unbounded
        }
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
            self.constraints[i].coefficients.push(1.0);
        }

        let initial_entering_variable_index = self.objective.len() - 1;
        let initial_leaving_variable_index = self
            .constraints
            .iter()
            .enumerate()
            .min_by(|(_, constraint_a), (_, constraint_b)| {
                constraint_a.coefficients[0]
                    .partial_cmp(&constraint_b.coefficients[0])
                    .expect("Expected basic variable coefficient not to be NaN.")
            })
            .expect("Expected to find a most infeasible constraint.")
            .0;

        self.pivot(
            initial_entering_variable_index,
            initial_leaving_variable_index,
        );
        assert!(self.pivot_until_solved()); // assume auxiliary problem is not unbounded

        if self.objective[0] != 0.0 {
            return false; // original program is infeasible
        }

        // transform back into feasible original program

        for i in 0..self.constraints.len() {
            self.constraints[i].coefficients.pop();
        }

        let mut restated_objective = vec![0.0; original_objective.len()];
        for i in 1..(1 + self.num_variables) {
            // TODO: loop from 0 for objectives with constant term
            match self
                .constraints
                .iter()
                .find(|constraint| constraint.index == i)
            {
                Some(constraint) => {
                    for j in 0..restated_objective.len() {
                        restated_objective[j] += original_objective[i] * constraint.coefficients[j];
                    }
                }
                None => {
                    restated_objective[i] += original_objective[i];
                }
            }
        }

        self.objective = restated_objective;
        true
    }

    fn select_entering_variable(&self) -> Option<usize> {
        for i in 1..self.objective.len() {
            if self.objective[i] > 0.0 {
                return Some(i);
            }
        }

        None
    }

    fn select_leaving_variable(&self, entering_variable_index: usize) -> Option<usize> {
        let mut leaving_variable_index = None;
        let mut least_upper_bound = std::f64::INFINITY;

        dbg!(entering_variable_index);

        for i in 0..self.constraints.len() {
            dbg!(i, self.constraints[i].coefficients[entering_variable_index]);

            let upper_bound = if self.constraints[i].coefficients[0] == 0.0
                && self.constraints[i].coefficients[entering_variable_index] == 0.0
            {
                0.0
            } else {
                self.constraints[i].coefficients[0]
                    / -self.constraints[i].coefficients[entering_variable_index]
            };

            dbg!(upper_bound);

            if self.constraints[i].coefficients[entering_variable_index] >= 0.0 {
                continue;
            }

            if leaving_variable_index.is_none() || upper_bound < least_upper_bound {
                leaving_variable_index = Some(i);
                least_upper_bound = upper_bound;
            }
        }

        dbg!(least_upper_bound);
        leaving_variable_index
    }

    fn pivot(&mut self, entering_variable_index: usize, leaving_variable_index: usize) {
        println!("Before pivot:\n{}", self);
        println!(
            "Entering variable: {}, leaving variable {}",
            entering_variable_index, leaving_variable_index
        );

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

        println!("After pivot:\n{}", self);
    }

    fn pivot_until_solved(&mut self) -> bool {
        while let Some(entering_variable_index) = self.select_entering_variable() {
            match self.select_leaving_variable(entering_variable_index) {
                Some(leaving_variable_index) => {
                    self.pivot(entering_variable_index, leaving_variable_index);
                }
                None => {
                    return false;
                }                
            }
        }

        true
    }
}

impl std::fmt::Display for LinearProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let column_width = 8;
        let label_width = 4;
        write!(f, "{:1$}", "", column_width + label_width + 1)?;
        for i in 0..self.num_variables {
            write!(f, " ")?;
            write!(f, "{:1$}", format!("x{}", i + 1), column_width)?;
        }
        for i in 0..self.constraints.len() {
            write!(f, " ")?;
            write!(f, "{:1$}", format!("w{}", i + 1), column_width)?;
        }
        if self.objective.len() > 1 + self.num_variables + self.constraints.len() {
            write!(f, " ")?;
            write!(f, "{:1$}", "x0", column_width)?;
        }
        writeln!(f)?;
        write!(f, "{:>1$}", "C =", label_width)?;
        for i in 0..self.objective.len() {
            write!(f, " ")?;
            write!(f, "{:1$.3}", self.objective[i], column_width)?;
        }
        writeln!(f)?;
        for i in 0..self.constraints.len() {
            if self.constraints[i].index == 0 {
                unreachable!();
            } else if self.constraints[i].index < 1 + self.num_variables {
                write!(
                    f,
                    "{:>1$}",
                    format!("x{} =", self.constraints[i].index),
                    label_width
                )?;
            } else if self.constraints[i].index < 1 + self.num_variables + self.constraints.len() {
                write!(
                    f,
                    "{:>1$}",
                    format!("w{} =", self.constraints[i].index - self.num_variables),
                    label_width
                )?;
            } else {
                write!(f, "{:>1$}", "x0 =", label_width)?;
            }

            for j in 0..self.constraints[i].coefficients.len() {
                write!(f, " ")?;
                write!(
                    f,
                    "{:1$.3}",
                    self.constraints[i].coefficients[j], column_width
                )?;
            }
            writeln!(f)?;
        }
        writeln!(f)
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

    #[test]
    fn phase_1_necessary() {
        let mut lp = LinearProgram::new(vec![-2.0, -1.0]);
        lp.add_constraint(vec![-1.0, 1.0], -1.0).unwrap();
        lp.add_constraint(vec![-1.0, -2.0], -2.0).unwrap();
        lp.add_constraint(vec![0.0, 1.0], 1.0).unwrap();

        let solution = lp.solve().unwrap();

        assert!(abs_diff_eq!(solution.0[0], 4.0 / 3.0));
        assert!(abs_diff_eq!(solution.0[1], 1.0 / 3.0));
        assert_eq!(solution.1, -3.0);
    }

    #[test]
    fn unbounded() {
        let mut lp = LinearProgram::new(vec![0.0, 2.0, 1.0]);
        lp.add_constraint(vec![1.0, -1.0, 1.0], 5.0).unwrap();
        lp.add_constraint(vec![-2.0, 1.0, 0.0], 3.0).unwrap();
        lp.add_constraint(vec![0.0, 1.0, -2.0], 5.0).unwrap();

        let solution = lp.solve().unwrap();

        assert_eq!(solution.0, vec![std::f64::INFINITY, std::f64::INFINITY, std::f64::INFINITY]);
        assert_eq!(solution.1, std::f64::INFINITY);
    }
}
