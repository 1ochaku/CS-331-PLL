import java.util.concurrent.locks.ReentrantLock;

public class Account {
    // declaring variables
    private final String accountNumber;
    private double balance;
    private final ReentrantLock lock;
    
    // initialisation when account called
    public Account(String accountNo, double initialBalance) {
        this.accountNumber = accountNo;
        this.balance = initialBalance;
        this.lock = new ReentrantLock();
    }

    // returns account number
    public String getAccountNumber() {
        return this.accountNumber;
    }
    
    //****** functions supported *********/ 
    // function 1: amount depositing
    // check if account exist and then add
    // TODO: if it doesn't, create a new account and add there
    public void depositMoney(double amount) {
        lock.lock();
        try {
            balance += amount;
        } finally {
            lock.unlock();
        }
    }
    
    // function 2: amount transfer
    // lock both the accounts involved in transaction and operate
    // TODO: check if the amount requested exists in the account
    public boolean transferMoney(Account recipient, double amount) {
        // since we should lock both accounts
        // now to avoid deadlock we follow a serial order that
        // whoever has account number less than will lock first
        // avoiding race conditions
        if (this == recipient) {
            // can't transfer to same account
            return false; 
        }
        if (this.accountNumber.compareTo(recipient.accountNumber) < 0) {
            this.lock.lock();
            recipient.lock.lock();
        } else {
            recipient.lock.lock();
            this.lock.lock();
        }
        try {
            if (amount > balance) {
                return false;
            }
            this.balance -= amount;
            recipient.balance += amount;
            return true;
        } finally {
            this.lock.unlock();
            recipient.lock.unlock();
        }
    }

    // function 3: amount withdrawal
    // withdraw the requested amount and update
    // TODO: withdrawal not allowed when account balance < req
    public boolean withdrawal(double amount) {
        lock.lock();
        try {
            if (amount > balance) {
                return false; // insufficient balance
            }
            balance -= amount;
            return true;
        } finally {
            lock.unlock();
        }
    }

    // function 4: balance check
    // shows the total amount
    public double getBalance() {
        lock.lock();
        try {
            return balance;
        } finally {
            lock.unlock();
        }
    }
}
