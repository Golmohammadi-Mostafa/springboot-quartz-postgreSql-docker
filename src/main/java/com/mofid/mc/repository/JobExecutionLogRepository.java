package com.mofid.mc.repository;

import com.mofid.mc.entity.JobExecutionLog;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface JobExecutionLogRepository extends JpaRepository<JobExecutionLog, Long> {

    List<JobExecutionLog> findAllByJobName(String jobName);
}
