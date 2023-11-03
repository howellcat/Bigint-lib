#ifndef BIGINT_H
#define BIGINT_H

#include <iostream>
#include <vector>
#include <algorithm>
#include <sstream>
#include <cmath>

class BigInt {
private:
    std::vector<int> digits;
    std::string value_;

    int short_BigInt_to_int(const BigInt anInt) const {
        // 将字符串表示的 BigInt 转换为 int
        int result = 0;
        bool isNegative = false;

        // 检查 BigInt 是否为负数
        if (value_[0] == '-') {
            isNegative = true;
        }

        // 从第一个非负号字符开始遍历
        for (size_t i = (isNegative ? 1 : 0); i < value_.size(); ++i) {
            // 将字符转换为数字
            int digit = value_[i] - '0';

            // 检查是否溢出
            if (result > INT_MAX / 10 || (result == INT_MAX / 10 && digit > INT_MAX % 10)) {
                if (isNegative) {
                    return INT_MIN;
                } else {
                    return INT_MAX;
                }
            }

            // 计算结果
            result = result * 10 + digit;
        }

        // 处理负数情况
        if (isNegative) {
            result = -result;
        }

        return result;
    }

public:
    BigInt() {}

    BigInt(long long n) {
        std::stringstream ss;
        ss << n;
        *this = BigInt(ss.str());
    }

    BigInt(const std::string &numStr) {
        for (char c: numStr) {
            if (std::isdigit(c)) {
                digits.push_back(c - '0');
            } else {
                throw std::invalid_argument("Invalid number format");
            }
        }
    }

    BigInt &operator=(const BigInt &other) {
        digits = other.digits;
        return *this;
    }

    bool operator<(const BigInt &other) const {
        if (digits.size() != other.digits.size()) {
            return digits.size() < other.digits.size();
        }

        for (int i = digits.size() - 1; i >= 0; i--) {
            if (digits[i] != other.digits[i]) {
                return digits[i] < other.digits[i];
            }
        }

        return false;
    }

    bool operator<=(const BigInt &other) const {
        return (*this < other) || (*this == other);
    }

    bool operator!=(const BigInt &other) const {
        return !(*this == other);
    }

    bool operator>=(const BigInt &other) const {
        return !(*this < other);
    }

    bool operator>(const BigInt &other) const {
        return !(*this <= other);
    }

    bool operator==(const BigInt &other) const {
        return digits == other.digits;
    }

    BigInt operator+(const BigInt &other) const {
        BigInt result;
        int carry = 0;
        int maxDigits = std::max(digits.size(), other.digits.size());

        for (int i = 0; i < maxDigits || carry; i++) {
            if (i < digits.size())
                carry += digits[i];
            if (i < other.digits.size())
                carry += other.digits[i];

            result.digits.push_back(carry % 10);
            carry /= 10;
        }

        return result;
    }

    BigInt operator-(const BigInt &other) const {
        BigInt result;
        int borrow = 0;
        int maxDigits = std::max(digits.size(), other.digits.size());

        for (int i = 0; i < maxDigits; i++) {
            int digit = digits[i] - borrow;
            if (i < other.digits.size())
                digit -= other.digits[i];

            if (digit < 0) {
                digit += 10;
                borrow = 1;
            } else {
                borrow = 0;
            }

            result.digits.push_back(digit);
        }

        // Remove leading zeros
        while (result.digits.size() > 1 && result.digits.back() == 0) {
            result.digits.pop_back();
        }

        return result;
    }

    BigInt operator*(const BigInt &other) const {
        BigInt result;
        int maxDigits = digits.size() + other.digits.size();
        result.digits.resize(maxDigits);

        for (int i = 0; i < digits.size(); i++) {
            int carry = 0;
            for (int j = 0; j < other.digits.size() || carry; j++) {
                long long product =
                        result.digits[i + j] +
                        (long long) digits[i] * (j < other.digits.size() ? other.digits[j] : 0) +
                        carry;
                result.digits[i + j] = product % 10;
                carry = product / 10;
            }
        }

        // Remove leading zeros
        while (result.digits.size() > 1 && result.digits.back() == 0) {
            result.digits.pop_back();
        }

        return result;
    }

    BigInt operator/(const BigInt &other) const {
        if (other == BigInt("0")) {
            throw std::runtime_error("Division by zero");
        }

        BigInt quotient;
        BigInt dividend = *this;
        BigInt divisor = other;

        while (dividend >= divisor) {
            BigInt partialQuotient;
            int shift = dividend.digits.size() - divisor.digits.size();
            BigInt shiftedDivisor = divisor;

            for (int i = 0; i < shift; i++) {
                shiftedDivisor.digits.insert(shiftedDivisor.digits.begin(), 0);
            }

            while (dividend >= shiftedDivisor) {
                dividend -= shiftedDivisor;
                partialQuotient += BigInt("1");
            }
            quotient += partialQuotient;
        }

        return quotient;
    }

    BigInt operator%(const BigInt &other) const {
        if (other == BigInt("0")) {
            throw std::runtime_error("Division by zero");
        }

        BigInt dividend = *this;
        BigInt divisor = other;

        while (dividend >= divisor) {
            BigInt quotient;
            BigInt partialQuotient;
            int shift = dividend.digits.size() - divisor.digits.size();
            BigInt shiftedDivisor = divisor;

            for (int i = 0; i < shift; i++) {
                shiftedDivisor.digits.insert(shiftedDivisor.digits.begin(), 0);
            }

            while (dividend >= shiftedDivisor) {
                dividend -= shiftedDivisor;
                partialQuotient += BigInt("1");
            }
            quotient += partialQuotient;
        }

        return dividend;
    }

    BigInt &operator+=(const BigInt &other) {
        int carry = 0;
        int maxDigits = std::max(digits.size(), other.digits.size());

        digits.resize(maxDigits, 0);  // 扩展当前数的位数

        for (int i = 0; i < maxDigits || carry; i++) {
            if (i < other.digits.size())
                digits[i] += other.digits[i] + carry;
            else
                digits[i] += carry;

            carry = digits[i] / 10;
            digits[i] %= 10;
        }

        return *this;
    }

    BigInt &operator-=(const BigInt &other) {
        int borrow = 0;
        int maxDigits = std::max(digits.size(), other.digits.size());

        digits.resize(maxDigits, 0);  // 扩展当前数的位数

        for (int i = 0; i < maxDigits; i++) {
            int digit = digits[i] - borrow;
            if (i < other.digits.size())
                digit -= other.digits[i];

            if (digit < 0) {
                digit += 10;
                borrow = 1;
            } else {
                borrow = 0;
            }

            digits[i] = digit;
        }

        // Remove leading zeros
        while (digits.size() > 1 && digits.back() == 0) {
            digits.pop_back();
        }

        return *this;
    }

    BigInt &operator*=(const BigInt &other) {
        BigInt result;
        int maxDigits = digits.size() + other.digits.size();
        result.digits.resize(maxDigits);

        for (int i = 0; i < digits.size(); i++) {
            int carry = 0;
            for (int j = 0; j < other.digits.size() || carry; j++) {
                long long product =
                        result.digits[i + j] +
                        (long long) digits[i] * (j < other.digits.size() ? other.digits[j] : 0) +
                        carry;
                result.digits[i + j] = product % 10;
                carry = product / 10;
            }
        }

        digits = result.digits;

        // Remove leading zeros
        while (digits.size() > 1 && digits.back() == 0) {
            digits.pop_back();
        }

        return *this;
    }

    BigInt &operator/=(const BigInt &other) {
        if (other == BigInt("0")) {
            throw std::runtime_error("Division by zero");
        }

        BigInt dividend = *this;
        BigInt divisor = other;
        BigInt quotient;
        BigInt remainder;

        // Initialize the quotient with zero
        quotient.digits.resize(dividend.digits.size(), 0);

        for (int i = dividend.digits.size() - 1; i >= 0; i--) {
            remainder = remainder * BigInt("10") + BigInt(std::to_string(dividend.digits[i]));

            if (remainder >= divisor) {
                BigInt partialQuotient = remainder / divisor;
                quotient.digits[i] = partialQuotient.digits[0];
                remainder = remainder - partialQuotient * divisor;
            }
        }

        *this = quotient;

        return *this;
    }

    BigInt &operator%=(const BigInt &other) {
        *this = *this % other;
        return *this;
    }

    BigInt operator++() {
        *this += BigInt("1");
        return *this;
    }

    BigInt operator--() {
        *this -= BigInt("1");
        return *this;
    }

    BigInt &operator++(int) {
        *this += BigInt("1");
        return *this;
    }

    BigInt &operator--(int) {
        *this -= BigInt("1");
        return *this;
    }

    friend std::ostream &operator<<(std::ostream &os, const BigInt &n) {
        for (int i = n.digits.size() - 1; i >= 0; i--) {
            os << n.digits[i];
        }

        return os;
    }

    friend std::istream &operator>>(std::istream &is, BigInt &n) {
        std::string input;
        is >> input;

        n.digits.clear();
        for (int i = input.size() - 1; i >= 0; i--) {
            n.digits.push_back(input[i] - '0');
        }

        return is;
    }

    int operator[](const BigInt &index) const {
        if (index < 0 || index >= digits.size()) {
            throw std::out_of_range("Index out of range");
        }

        return digits[short_BigInt_to_int(index)];
    }

    int &operator[](const BigInt &index) {
        if (index < 0 || index >= digits.size())
            throw std::out_of_range("Index out of range");


        return digits[short_BigInt_to_int(index)];
    }

    static BigInt sqrt(const BigInt &n) {
        if (n < 0) {
            throw std::domain_error("Square root of negative number is not defined");
        }

        if (n == 0 || n == 1) {
            return n;
        }

        BigInt start = 1;
        BigInt end = n;
        BigInt result;

        while (start <= end) {
            BigInt mid = (start + end) / BigInt(2);
            BigInt sqr = mid * mid;

            if (sqr == n) {
                return mid;
            } else if (sqr < n) {
                start = mid + 1;
                result = mid;
            } else {
                end = mid - 1;
            }
        }

        return result;
    }

    static BigInt pow(const BigInt &base, const BigInt &exponent) {
        if (exponent < 0) {
            throw std::invalid_argument("Negative exponent is not supported");
        }

        if (exponent == 0) {
            return 1;
        }

        BigInt result = 1;
        BigInt b = base;
        BigInt exp = exponent;

        while (exp > 0) {
            if (exp % 2 == 1) {
                result *= b;
            }

            b *= b;
            exp /= 2;
        }

        return result;
    }

    static BigInt factorial(const BigInt &n) {
        if (n < 0) {
            throw std::invalid_argument("Negative number is not supported");
        }

        BigInt result = 1;

        for (BigInt i = 2; i <= n; i++) {
            result *= i;
        }

        return result;
    }

    static BigInt combination(const BigInt &n, const BigInt &k) {
        if (n < 0 || k < 0 || k > n) {
            throw std::invalid_argument("Invalid arguments");
        }

        BigInt result = factorial(n) / (factorial(k) * factorial(n - k));

        return result;
    }

    static BigInt cos(const BigInt &angle) {
        int numTerms = 10;  // 泰勒级数展开的项数
        BigInt result = 1;
        BigInt term = 1;
        BigInt factorial = 1;
        BigInt angleSquared = angle * angle;
        int sign = -1;

        for (int i = 1; i <= numTerms; i++) {
            term *= angleSquared;
            factorial *= (2 * i - 1) * (2 * i);
            term /= factorial;
            term *= sign;
            result += term;
            sign *= -1;
        }

        return result;
    }

    static BigInt sin(const BigInt &angle) {
        int numTerms = 10;  // 泰勒级数展开的项数
        BigInt result = angle;
        BigInt term = angle;
        BigInt factorial = 1;
        BigInt angleSquared = angle * angle;
        int sign = -1;

        for (int i = 1; i <= numTerms; i++) {
            term *= angleSquared;
            factorial *= (2 * i) * (2 * i + 1);
            term /= factorial;
            term *= sign;
            result += term;
            sign *= -1;
        }
        return result;
    }

    int short_BigInt_to_int() {
        // 将字符串表示的 BigInt 转换为 int
        int result = 0;
        bool isNegative = false;

        // 检查 BigInt 是否为负数
        if (value_[0] == '-') {
            isNegative = true;
        }

        // 从第一个非负号字符开始遍历
        for (size_t i = (isNegative ? 1 : 0); i < value_.size(); ++i) {
            // 将字符转换为数字
            int digit = value_[i] - '0';

            // 检查是否溢出
            if (result > INT_MAX / 10 || (result == INT_MAX / 10 && digit > INT_MAX % 10)) {
                if (isNegative) {
                    return INT_MIN;
                } else {
                    return INT_MAX;
                }
            }

            // 计算结果
            result = result * 10 + digit;
        }

        // 处理负数情况
        if (isNegative) {
            result = -result;
        }

        return result;
    }

};

#endif  // BIGINT_H